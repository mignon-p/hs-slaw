{-
  NumericConvert.hs is generated from this template.  The "--FOR"
  and "--END" directives can be used to loop over a set of types.
  The available sets of types are:

  sizedInt  - Int8, Int16, Int32, Int64, Word8, Word16, Word32, Word64
  nativeInt - Int, Word
  floating  - Float, Double
  vectors   - V2 V3 V4

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
  VTYPE  - Vt2/Vt3/Vt4 for V2/V3/V4, respectively (only for vectors)
-}

{-# LANGUAGE ScopedTypeVariables        #-}

module Data.Slaw.Internal.NumericConvert
  ( RealClass(..)
  , ScalarClass(..)
  , NumericClass(..)
  ) where

-- import Control.Arrow (second)
import Control.DeepSeq
import Data.Complex
import Data.Default.Class
import Data.Hashable
import Data.Int
import Data.List
import qualified Data.Vector.Storable as S
import Data.Word
import Foreign.Storable
import GHC.Generics (Generic)

import Data.Slaw.Internal.Exception
import Data.Slaw.Internal.Nameable
import Data.Slaw.Internal.NativeInt
import Data.Slaw.Internal.NumericTypes
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

class (Storable a, Num a, Nameable a) => RealClass a where
  ndToReal :: Maybe String
           -> (NumericFormat, NumericData)
           -> Either PlasmaException (S.Vector a)

  realToNd :: S.Vector a
           -> (NumericFormat, NumericData)

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

--FOR nativeInt

instance RealClass TYPE where
  ndToReal _ = mapRight f . ndToReal (Just "TYPE")
    where f :: S.Vector NativeTYPE -> S.Vector TYPE
          f = S.unsafeCast

  realToNd = realToNd . f
    where f :: S.Vector TYPE -> S.Vector NativeTYPE
          f = S.unsafeCast

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

--END

cnfScalar :: CheckNF
cnfScalar = CheckNF
  { cnfArray   = Nothing
  , cnfComplex = Nothing
  , cnfVector  = Just VtScalar
  }

-- This handles casting a real number to a Complex number,
-- by setting the imaginary part to 0.
insertZeros :: (Storable a, Num a) => S.Vector a -> S.Vector a
insertZeros v = S.generate len' f
  where len' = 2 * S.length v
        f i  = let (q, r) = i `divMod` 2
               in if r == 0
                  then v S.! q
                  else 0

class (Storable a, Nameable a) => ScalarClass a where
  ndToScalar :: (NumericFormat, NumericData)
             -> Either PlasmaException (S.Vector a)

  scalarToNd :: S.Vector a
             -> (NumericFormat, NumericData)

instance RealClass a => ScalarClass (Complex a) where
  ndToScalar (nf, nd) = do
    let toType   = typeName (undefined :: Complex a)
        realNF   = nf { nfComplex = False }
        singleNF = nf { nfArray   = False }
    checkNF cnfScalar nf (singleNF, nd, toType)
    v1 <- case ndToReal Nothing (realNF, nd) of
            Left err ->
              let msg = describeNumeric singleNF nd `cantCoerce` toType
              in msg `because` [err]
            Right v0 -> return v0
    let v2 = if nfComplex nf
             then v1
             else insertZeros v1
        f :: S.Vector a -> S.Vector (Complex a)
        f  = S.unsafeCast
    return (f v2)

  scalarToNd v = (nf', nd)
    where
      f       :: S.Vector (Complex a) -> S.Vector a
      f        = S.unsafeCast
      v'       = f v
      (nf, nd) = realToNd v'
      nf'      = nf { nfComplex = True }

--FOR sizedInt, nativeInt, floating

instance ScalarClass TYPE where
  ndToScalar = ndToReal Nothing
  scalarToNd = realToNd

--END

class (Storable a, Nameable a) => NumericClass a where
  ndToNumeric :: (NumericFormat, NumericData)
              -> Either PlasmaException (S.Vector a)

  numericToNd :: S.Vector a
              -> (NumericFormat, NumericData)

--FOR vectors

instance ScalarClass a => NumericClass (TYPE a) where
  ndToNumeric (nf, nd) = do
    let toType    = typeName (undefined :: TYPE a)
        scalarNF  = nf { nfVector = VtScalar }
        singleNF  = nf { nfArray  = False    }
        cnf       = cnfScalar { cnfVector = Just VTYPE }
    checkNF cnf nf (singleNF, nd, toType)
    v <- case ndToScalar (scalarNF, nd) of
           Left err ->
             let msg = describeNumeric singleNF nd `cantCoerce` toType
             in msg `because` [err]
           Right v0 -> return v0
    let f :: S.Vector a -> S.Vector (TYPE a)
        f  = S.unsafeCast
    return (f v)

  numericToNd v = (nf', nd)
    where
      f       :: S.Vector (TYPE a) -> S.Vector a
      f        = S.unsafeCast
      v'       = f v
      (nf, nd) = scalarToNd v'
      nf'      = nf { nfVector = VTYPE }

--END
