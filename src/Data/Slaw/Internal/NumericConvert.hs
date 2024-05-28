{- GENERATED FILE - DO NOT EDIT

   To modify, edit the template file, and re-run the script.

   template: scripts/NumericConvert-template.hs
     script: scripts/gen-NumericConvert.pl
     output: src/Data/Slaw/Internal/NumericConvert.hs

   Script takes no arguments.  It finds its input and output
   files relative to its own position in the source tree.
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
numTypeName (NumInt8   _) = "Int8"
numTypeName (NumInt16  _) = "Int16"
numTypeName (NumInt32  _) = "Int32"
numTypeName (NumInt64  _) = "Int64"
numTypeName (NumUnt8   _) = "Word8"
numTypeName (NumUnt16  _) = "Word16"
numTypeName (NumUnt32  _) = "Word32"
numTypeName (NumUnt64  _) = "Word64"
numTypeName (NumFloat  _) = "Float"
numTypeName (NumDouble _) = "Double"

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

instance RealClass Int8 where
  ndToReal tname (nf, nd) = do
    let fromType = numTypeName nd
        toType   = tname ?> "Int8"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumInt8   v -> return v
      _           -> do
        let typePair = (fromType, toType)
        nes <- mapM (checkRange' typePair (0 :: Int8)) $ numToList nd
        return $ S.fromList $ map intCoerce nes

  realToNd = (def,) . NumInt8

instance RealClass Int16 where
  ndToReal tname (nf, nd) = do
    let fromType = numTypeName nd
        toType   = tname ?> "Int16"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumInt16  v -> return v
      _           -> do
        let typePair = (fromType, toType)
        nes <- mapM (checkRange' typePair (0 :: Int16)) $ numToList nd
        return $ S.fromList $ map intCoerce nes

  realToNd = (def,) . NumInt16

instance RealClass Int32 where
  ndToReal tname (nf, nd) = do
    let fromType = numTypeName nd
        toType   = tname ?> "Int32"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumInt32  v -> return v
      _           -> do
        let typePair = (fromType, toType)
        nes <- mapM (checkRange' typePair (0 :: Int32)) $ numToList nd
        return $ S.fromList $ map intCoerce nes

  realToNd = (def,) . NumInt32

instance RealClass Int64 where
  ndToReal tname (nf, nd) = do
    let fromType = numTypeName nd
        toType   = tname ?> "Int64"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumInt64  v -> return v
      _           -> do
        let typePair = (fromType, toType)
        nes <- mapM (checkRange' typePair (0 :: Int64)) $ numToList nd
        return $ S.fromList $ map intCoerce nes

  realToNd = (def,) . NumInt64

instance RealClass Word8 where
  ndToReal tname (nf, nd) = do
    let fromType = numTypeName nd
        toType   = tname ?> "Word8"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumUnt8   v -> return v
      _           -> do
        let typePair = (fromType, toType)
        nes <- mapM (checkRange' typePair (0 :: Word8)) $ numToList nd
        return $ S.fromList $ map intCoerce nes

  realToNd = (def,) . NumUnt8

instance RealClass Word16 where
  ndToReal tname (nf, nd) = do
    let fromType = numTypeName nd
        toType   = tname ?> "Word16"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumUnt16  v -> return v
      _           -> do
        let typePair = (fromType, toType)
        nes <- mapM (checkRange' typePair (0 :: Word16)) $ numToList nd
        return $ S.fromList $ map intCoerce nes

  realToNd = (def,) . NumUnt16

instance RealClass Word32 where
  ndToReal tname (nf, nd) = do
    let fromType = numTypeName nd
        toType   = tname ?> "Word32"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumUnt32  v -> return v
      _           -> do
        let typePair = (fromType, toType)
        nes <- mapM (checkRange' typePair (0 :: Word32)) $ numToList nd
        return $ S.fromList $ map intCoerce nes

  realToNd = (def,) . NumUnt32

instance RealClass Word64 where
  ndToReal tname (nf, nd) = do
    let fromType = numTypeName nd
        toType   = tname ?> "Word64"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumUnt64  v -> return v
      _           -> do
        let typePair = (fromType, toType)
        nes <- mapM (checkRange' typePair (0 :: Word64)) $ numToList nd
        return $ S.fromList $ map intCoerce nes

  realToNd = (def,) . NumUnt64

instance RealClass Int where
  ndToReal _ = mapRight f . ndToReal (Just "Int")
    where f :: S.Vector NativeInt -> S.Vector Int
          f = S.unsafeCast

  realToNd = realToNd . f
    where f :: S.Vector Int -> S.Vector NativeInt
          f = S.unsafeCast

instance RealClass Word where
  ndToReal _ = mapRight f . ndToReal (Just "Word")
    where f :: S.Vector NativeWord -> S.Vector Word
          f = S.unsafeCast

  realToNd = realToNd . f
    where f :: S.Vector Word -> S.Vector NativeWord
          f = S.unsafeCast

instance RealClass Float where
  ndToReal tname (nf, nd) = do
    let toType   = tname ?> "Float"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumFloat  v -> return v
      _           -> do
        let nes = numToList nd
        return $ S.fromList $ map floatCoerce nes

  realToNd = (def,) . NumFloat

instance RealClass Double where
  ndToReal tname (nf, nd) = do
    let toType   = tname ?> "Double"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumDouble v -> return v
      _           -> do
        let nes = numToList nd
        return $ S.fromList $ map doubleCoerce nes

  realToNd = (def,) . NumDouble

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

instance ScalarClass Int8 where
  ndToScalar = ndToReal Nothing
  scalarToNd = realToNd

instance ScalarClass Int16 where
  ndToScalar = ndToReal Nothing
  scalarToNd = realToNd

instance ScalarClass Int32 where
  ndToScalar = ndToReal Nothing
  scalarToNd = realToNd

instance ScalarClass Int64 where
  ndToScalar = ndToReal Nothing
  scalarToNd = realToNd

instance ScalarClass Word8 where
  ndToScalar = ndToReal Nothing
  scalarToNd = realToNd

instance ScalarClass Word16 where
  ndToScalar = ndToReal Nothing
  scalarToNd = realToNd

instance ScalarClass Word32 where
  ndToScalar = ndToReal Nothing
  scalarToNd = realToNd

instance ScalarClass Word64 where
  ndToScalar = ndToReal Nothing
  scalarToNd = realToNd

instance ScalarClass Int where
  ndToScalar = ndToReal Nothing
  scalarToNd = realToNd

instance ScalarClass Word where
  ndToScalar = ndToReal Nothing
  scalarToNd = realToNd

instance ScalarClass Float where
  ndToScalar = ndToReal Nothing
  scalarToNd = realToNd

instance ScalarClass Double where
  ndToScalar = ndToReal Nothing
  scalarToNd = realToNd

class (Storable a, Nameable a) => NumericClass a where
  ndToNumeric :: (NumericFormat, NumericData)
              -> Either PlasmaException (S.Vector a)

  numericToNd :: S.Vector a
              -> (NumericFormat, NumericData)

instance ScalarClass a => NumericClass (V2 a) where
  ndToNumeric (nf, nd) = do
    let toType    = typeName (undefined :: V2 a)
        scalarNF  = nf { nfVector = VtScalar }
        singleNF  = nf { nfArray  = False    }
        cnf       = cnfScalar { cnfVector = Just Vt2 }
    checkNF cnf nf (singleNF, nd, toType)
    v <- case ndToScalar (scalarNF, nd) of
           Left err ->
             let msg = describeNumeric singleNF nd `cantCoerce` toType
             in msg `because` [err]
           Right v0 -> return v0
    let f :: S.Vector a -> S.Vector (V2 a)
        f  = S.unsafeCast
    return (f v)

  numericToNd v = (nf', nd)
    where
      f       :: S.Vector (V2 a) -> S.Vector a
      f        = S.unsafeCast
      v'       = f v
      (nf, nd) = scalarToNd v'
      nf'      = nf { nfVector = Vt2 }

instance ScalarClass a => NumericClass (V3 a) where
  ndToNumeric (nf, nd) = do
    let toType    = typeName (undefined :: V3 a)
        scalarNF  = nf { nfVector = VtScalar }
        singleNF  = nf { nfArray  = False    }
        cnf       = cnfScalar { cnfVector = Just Vt3 }
    checkNF cnf nf (singleNF, nd, toType)
    v <- case ndToScalar (scalarNF, nd) of
           Left err ->
             let msg = describeNumeric singleNF nd `cantCoerce` toType
             in msg `because` [err]
           Right v0 -> return v0
    let f :: S.Vector a -> S.Vector (V3 a)
        f  = S.unsafeCast
    return (f v)

  numericToNd v = (nf', nd)
    where
      f       :: S.Vector (V3 a) -> S.Vector a
      f        = S.unsafeCast
      v'       = f v
      (nf, nd) = scalarToNd v'
      nf'      = nf { nfVector = Vt3 }

instance ScalarClass a => NumericClass (V4 a) where
  ndToNumeric (nf, nd) = do
    let toType    = typeName (undefined :: V4 a)
        scalarNF  = nf { nfVector = VtScalar }
        singleNF  = nf { nfArray  = False    }
        cnf       = cnfScalar { cnfVector = Just Vt4 }
    checkNF cnf nf (singleNF, nd, toType)
    v <- case ndToScalar (scalarNF, nd) of
           Left err ->
             let msg = describeNumeric singleNF nd `cantCoerce` toType
             in msg `because` [err]
           Right v0 -> return v0
    let f :: S.Vector a -> S.Vector (V4 a)
        f  = S.unsafeCast
    return (f v)

  numericToNd v = (nf', nd)
    where
      f       :: S.Vector (V4 a) -> S.Vector a
      f        = S.unsafeCast
      v'       = f v
      (nf, nd) = scalarToNd v'
      nf'      = nf { nfVector = Vt4 }

