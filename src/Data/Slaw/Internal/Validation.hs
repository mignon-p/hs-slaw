module Data.Slaw.Internal.Validation
  ( ValidationFlag(..)
  , ValidationFlags
  , validateSlaw
  ) where

import Control.DeepSeq
-- import Control.Exception
import Data.Bits
-- import qualified Data.ByteString.Lazy as L
-- import Data.Default.Class
import Data.Hashable
import GHC.Generics (Generic)

import Data.Slaw.Internal.Exception
import Data.Slaw.Internal.Helpers
import Data.Slaw.Internal.SlawEncode
import Data.Slaw.Internal.SlawType
import Data.Slaw.Internal.String

data ValidationFlag = VfCSlaw | VfUtf8 | VfDesIng
                    deriving (Eq, Ord, Show, Read, Bounded, Enum,
                              Generic, NFData, Hashable)

type ValidationFlags = [ValidationFlag]
type ValRet          = Either PlasmaException ()

-- FIXME: use bitfield definition instead
symbolBits :: Int
symbolBits = 56

maxSymbol :: Symbol
maxSymbol = bit symbolBits - 1

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
  | n <= (fromIntegral . fromEnum) SymError =
      valErr $ concat [ symNum
                      , " is reserved for "
                      , (drop 3 . show) (sym :: Sym)
                      ]
  | n > maxSymbol                           =
      valErr $ concat [ symNum
                      , " does not fit in "
                      , show symbolBits
                      , " bits"
                      ]
  | otherwise                               = return ()
  where sym    = (toEnum . fromIntegral) n
        symNum = "symbol " ++ show n

vsString :: Utf8Str -> ValRet
vsString = undefined

vsPair :: ValidationFlags -> (Slaw, Slaw) -> ValRet
vsPair = undefined

vsNumeric :: ValidationFlags -> NumericFormat -> NumericData -> ValRet
vsNumeric = undefined

vsError :: String -> ErrLocation -> ValRet
vsError = undefined

cslawErr :: String -> ValRet
cslawErr feature = valErr $ feature ++ " not supported by c-plasma"

valErr :: String -> ValRet
valErr = undefined

validateSlaw :: ValidationFlags -> Slaw -> Either PlasmaException ()
validateSlaw = vs
