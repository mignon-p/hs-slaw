{-|
Module      : Data.Slaw.Internal.Validation
Description : Validate that a Slaw does not contain errors
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module Data.Slaw.Internal.Validation
  ( ValidationFlag(..)
  , validateSlaw
  , minSymbol
  , maxSymbol
  ) where

import Control.DeepSeq
-- import Control.Exception
import Control.Monad
import Data.Bits
-- import qualified Data.ByteString.Lazy     as L
import Data.Default.Class
import Data.Hashable
-- import qualified Data.Text                as T
-- import qualified Data.Text.Encoding       as T
-- import qualified Data.Text.Encoding.Error as T
-- import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.Encoding  as LT
import GHC.Generics (Generic)

import Data.Slaw.Internal.Exception
import Data.Slaw.Internal.Helpers
import Data.Slaw.Internal.SlawEncode
import Data.Slaw.Internal.SlawType
import Data.Slaw.Internal.String

-- | Flags which can be passed to modify the behavior of 'validateSlaw'.
data ValidationFlag =
    VfCSlaw -- ^ Fail if slaw uses features not supported by @libPlasma/c@
  | VfUtf8  -- ^ Fail if 'SlawString' is not valid UTF-8
  | VfDesIng -- ^ Fail if descrips is not a list, or ingests is not a map
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, NFData, Hashable)

type ValidationFlags = [ValidationFlag]
type ValRet          = Either PlasmaException ()

-- FIXME: use bitfield definition instead
symbolBits :: Int
symbolBits = 56

-- | The maximum value a 'Symbol' is allowed to have.
-- (@0x00ff_ffff_ffff_ffff@)
maxSymbol :: Symbol
maxSymbol = bit symbolBits - 1

-- | The minimum value a 'Symbol' is allowed to have.
-- (@4@)
minSymbol :: Symbol
minSymbol = 1 + (fromIntegral . fromEnum) SymError

vs :: ValidationFlags -> Slaw -> ValRet
vs f     (SlawProtein d i _  ) = vsProtein f d i
vs _     (SlawBool    _      ) = return ()
vs _      SlawNil              = return ()
vs f     (SlawList    ss     ) = mapM_ (vs     f) ss
vs f     (SlawMap     pairs  ) = mapM_ (vsPair f) pairs
vs f     (SlawCons    car cdr) = vsPair f (car, cdr)
vs f     (SlawNumeric nf  nd ) = vsNumeric f nf nd
vs _     (SlawError   msg loc) = vsError   msg loc
vs f     (SlawSymbol  sym    )
  | VfCSlaw `elem` f           = cslawErr "symbols"
  | otherwise                  = vsSymbol sym
vs f     (SlawString  utf8   )
  | VfUtf8 `elem` f            = vsString utf8
  | otherwise                  = return ()

vsProtein :: ValidationFlags
          -> Maybe Slaw
          -> Maybe Slaw
          -> ValRet
vsProtein vf des ing = do
  let validateTypes = VfDesIng `elem` vf
  vsp1 vf validateTypes des "descrips" "list" isList
  vsp1 vf validateTypes ing "ingests"  "map"  isMap

vsp1 :: ValidationFlags
     -> Bool
     -> Maybe Slaw
     -> String
     -> String
     -> (Slaw -> Bool)
     -> ValRet
vsp1 _  _  Nothing  _    _        _    = return ()
vsp1 vf vt (Just s) what expected predicate =
  if vt && not (predicate s)
  then valErr $ concat [ what
                       , " is "
                       , describeSlaw s
                       , ", but expected "
                       , expected
                       ]
  else vs vf s

vsSymbol :: Symbol -> ValRet
vsSymbol n
  | n < minSymbol =
      valErr $ concat [ symNum
                      , " is reserved for "
                      , (drop 3 . show) (sym :: Sym)
                      ]
  | n > maxSymbol =
      valErr $ concat [ symNum
                      , " does not fit in "
                      , show symbolBits
                      , " bits"
                      ]
  | otherwise                               = return ()
  where sym    = (toEnum . fromIntegral) n
        symNum = "symbol " ++ show n

vsString :: Utf8Str -> ValRet
vsString utf8 =
  case LT.decodeUtf8' utf8 of
    Left ue -> Left $ unicodeError1 ue
    Right _ -> Right ()

vsPair :: ValidationFlags -> (Slaw, Slaw) -> ValRet
vsPair vf (car, cdr) = do
  vs vf car
  vs vf cdr

vsNumeric :: ValidationFlags -> NumericFormat -> NumericData -> ValRet
vsNumeric vf nf nd = do
  when (not $ isNumericFormatLegal nf) $ do
    valErr $ "invalid numeric format: " ++ describeNumericFormat nf
  when (VfCSlaw `elem` vf) $ vsnC nd
  let (q, r) = nElems `divMod` bsize
      nElems = lengthNumericData nd
      bsize  = numericFormatSize nf
  when (r /= 0) $ do
    valErr $ concat [ "in "
                    , describeNumeric nf nd
                    , ", length "
                    , show nElems
                    , " is not divisible by "
                    , show bsize
                    ]
  when (not (nfArray nf) && q /= 1) $ do
    valErr $ concat [ describeNumeric nf nd
                    , " has "
                    , show q
                    , " elements, but is not an array"
                    ]

vsnC :: NumericData -> ValRet
vsnC (NumFloat16 _) = cslawErr "16-bit floating point"
vsnC _              = return ()

vsError :: String -> ErrLocation -> ValRet
vsError msg loc =
  Left $ def { peType     = etFromMsg msg
             , peMessage  = msg
             , peLocation = Just loc
             }

cslawErr :: String -> ValRet
cslawErr feature = valErr $ feature ++ " not supported by c-plasma"

valErr :: String -> ValRet
valErr = Left . validationError

-- | Validates the following:
--
-- * 'SlawError' does not appear
-- * 'NumericFormat' passes 'isNumericFormatLegal'
-- * 'NumericData' has a legal number of elements for the given 'NumericFormat'
-- * 'Symbol' is between 'minSymbol' and 'maxSymbol', inclusive
--
-- If one or more 'ValidationFlag' is specified, will validate
-- additional properties, as well.
validateSlaw :: [ValidationFlag] -> Slaw -> Either PlasmaException ()
validateSlaw = vs
