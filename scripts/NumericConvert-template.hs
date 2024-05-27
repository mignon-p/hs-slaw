{-
  NumericConvert.hs is generated from this template.  The "--FOR"
  and "--END" directives can be used to loop over a set of types.
  The available sets of types are:

  sizedInt  - Int8, Int16, Int32, Int64, Word8, Word16, Word32, Word64
  nativeInt - Int, Word
  floating  - Float, Double

  More than one set can be specified in a "--FOR" directive,
  separated by commas.

  "--FOR" directives do not nest, so one "--FOR" loop can be ended
  by starting another "--FOR" loop.  Or, a "--FOR" loop can be
  ended without starting another loop, with the "--END" directive.

  The following strings will be replaced in the body of the "--FOR":

  TYPE   - the name of the type
  LTYPE  - same as TYPE, but all lowercase
  NAME   - same as TYPE, but "Word" is replaced with "Unt"
  NAMEXX - same as NAME, but padded with spaces to be 6 characters long
-}

{-# LANGUAGE ScopedTypeVariables        #-}

module Data.Slaw.Internal.NumericConvert
  ( RealClass(..)
  , ScalarClass(..)
  ) where

-- import Control.Arrow (second)
import Control.DeepSeq
-- import Data.Complex
import Data.Default.Class
import Data.Hashable
import Data.Int
import Data.List
import qualified Data.Vector.Storable as S
import Data.Word
import Foreign.Storable
import GHC.Generics (Generic)

import Data.Slaw.Internal.Exception
import Data.Slaw.Internal.NativeInt
import Data.Slaw.Internal.SlawType
import Data.Slaw.Internal.Util

data CheckNF = CheckNF
  { cnfArray   :: Maybe Bool
  , cnfComplex :: Maybe Bool
  , cnfVector  :: Maybe VectorType
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)

checkNF :: CheckNF
        -> NumericFormat
        -> (NumericFormat, NumericData, String)
        -> Either PlasmaException ()
checkNF cnf nf (fromTypeNF, fromTypeND, toType) =
  let aOK      = fmap (== nfArray   nf) (cnfArray   cnf) ?> True
      cOK      = fmap (== nfComplex nf) (cnfComplex cnf) ?> True
      vOK      = fmap (== nfVector  nf) (cnfVector  cnf) ?> True
      fromType = describeNumeric fromTypeNF fromTypeND
  in if aOK && cOK && vOK
     then Right ()
     else Left $ typeMismatch $ concat [ "Can't convert"
                                       , fromType
                                       , " to "
                                       , toType
                                       ]

describeNumeric :: NumericFormat -> NumericData -> String
describeNumeric nf nd = intercalate " " (nfl ++ [nds])
  where nfl = describeNumericFormat nf
        nds = describeNumericData   nd

rangeErr :: Show a
         => (String, String)
         -> a
         -> (Integer, Integer)
         -> Either PlasmaException b
rangeErr (fromType, toType) i (lo, hi) =
  Left $ rangeError' ( fromType
                     , show i
                     , toType
                     , show lo
                     , show hi
                     )

checkRange :: (String, String)
           -> (Integer, Integer)
           -> NumElem
           -> Either PlasmaException NumElem
checkRange typs (lo, hi) x@(ElemInt i)
  | lo <= i && i <= hi                         = Right x
  | otherwise                                  = rangeErr typs i (lo, hi)
checkRange typs (lo, hi) x@(ElemFloat f)
  | fromInteger lo <= f && f <= fromInteger hi = Right x
  | otherwise                                  = rangeErr typs f (lo, hi)
checkRange typs (lo, hi) x@(ElemDouble d)
  | fromInteger lo <= d && d <= fromInteger hi = Right x
  | otherwise                                  = rangeErr typs d (lo, hi)

checkRange' :: forall a. (Integral a, Bounded a)
            => (String, String)
            -> a
            -> NumElem
            -> Either PlasmaException NumElem
checkRange' typs _ = checkRange typs range
  where range = (toInteger (minBound :: a), toInteger (maxBound :: a))

numTypeName :: NumericData -> String
--FOR sizedInt, floating
numTypeName (NumNAMEXX _) = "TYPE"
--END

cnfReal :: CheckNF
cnfReal = CheckNF
  { cnfArray   = Nothing
  , cnfComplex = Just False
  , cnfVector  = Just VtScalar
  }

class (Storable a, Num a) => RealClass a where
  ndToReal :: Maybe String
           -> (NumericFormat, NumericData)
           -> Either PlasmaException (S.Vector a)

  realToNd :: S.Vector a
           -> (NumericFormat, NumericData)

  realName :: a -> String

--FOR sizedInt

instance RealClass TYPE where
  ndToReal tname (nf, nd) = do
    let fromType = numTypeName nd
        toType   = tname ?> "TYPE"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumNAMEXX v -> return v
      _           -> do
        let typePair = (fromType, toType)
        nes <- mapM (checkRange' typePair (0 :: TYPE)) $ numToList nd
        return $ S.fromList $ map intCoerce nes

  realToNd = (def,) . NumNAME

  realName _ = "TYPE"

--FOR nativeInt

instance RealClass TYPE where
  ndToReal _ = mapRight f . ndToReal (Just "TYPE")
    where f :: S.Vector NativeTYPE -> S.Vector TYPE
          f = S.unsafeCast

  realToNd = realToNd . f
    where f :: S.Vector TYPE -> S.Vector NativeTYPE
          f = S.unsafeCast

  realName _ = "TYPE"

--FOR floating

instance RealClass TYPE where
  ndToReal tname (nf, nd) = do
    let toType   = tname ?> "TYPE"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumNAMEXX v -> return v
      _           -> do
        let nes = numToList nd
        return $ S.fromList $ map LTYPECoerce nes

  realToNd = (def,) . NumNAME

  realName _ = "TYPE"

--END

{-
cnfScalar :: CheckNF
cnfScalar = CheckNF
  { cnfArray   = Nothing
  , cnfComplex = Nothing
  , cnfVector  = Just VtScalar
  }
-}

class Storable a => ScalarClass a where
  ndToScalar :: Maybe String
             -> (NumericFormat, NumericData)
             -> Either PlasmaException (S.Vector a)

  scalarToNd :: S.Vector a
             -> (NumericFormat, NumericData)

  scalarName :: a -> String

{-
instance RealClass a => ScalarClass (Complex a) where
  ndToScalar tname (nf, nd) = do
    let toType   = tname ?> ("Complex " ++ realName (undefined :: a))
        realNF   = nf { nfComplex = False }
        singleNF = nf { nfArray   = False }
    checkNF cnfScalar nf (singleNF, nd, toType)
    v   <- ndToReal Nothing (realNF, nd)
    nd1 <- case v of
             Left err ->
               let msg = describeNumeric singleNF nd `cantCoerce` toType
               in msg `because` [err]
             Right (_, nd0) -> return nd0
    let nd2 = if nfComplex nf
              then nd1
              else insertZeros nd1
    undefined
-}
