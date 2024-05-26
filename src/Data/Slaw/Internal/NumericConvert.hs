{-# LANGUAGE ScopedTypeVariables        #-}

module Data.Slaw.Internal.NumericConvert
  ( RealClass(..)
  , ScalarClass(..)
  ) where

import Control.Arrow (second)
import Control.DeepSeq
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
      fromType = intercalate " " (nfl ++ [nds])
      nfl      = describeNumericFormat fromTypeNF
      nds      = describeNumericData   fromTypeND
  in if aOK && cOK && vOK
     then Right ()
     else Left $ typeMismatch $ concat [ "Can't convert"
                                       , fromType
                                       , " to "
                                       , toType
                                       ]

rangeErr :: Show a
         => (String, String)
         -> a
         -> (Integer, Integer)
         -> Either PlasmaException b
rangeErr (fromType, toType) i (lo, hi) =
  Left $ typeMismatch $ concat [ fromType
                               , " "
                               , show i
                               , " is not in the range of "
                               , toType
                               , " ["
                               , show lo
                               , ".."
                               , show hi
                               , "]"
                               ]

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

{-
numType :: NumericData -> NumericType
numType (NumInt8   _) = TypInt8
numType (NumInt16  _) = TypInt16
numType (NumInt32  _) = TypInt32
numType (NumInt64  _) = TypInt64
numType (NumUnt8   _) = TypUnt8
numType (NumUnt16  _) = TypUnt16
numType (NumUnt32  _) = TypUnt32
numType (NumUnt64  _) = TypUnt64
numType (NumFloat  _) = TypFloat
numType (NumDouble _) = TypDouble
-}

cnfReal :: CheckNF
cnfReal = CheckNF
  { cnfArray   = Nothing
  , cnfComplex = Just False
  , cnfVector  = Just VtScalar
  }

class (Storable a, Num a) => RealClass a where
  ndToReal :: Maybe String
           -> (NumericFormat, NumericData)
           -> Either PlasmaException (NumericFormat, S.Vector a)

  realToNd :: Maybe String
           -> (NumericFormat, S.Vector a)
           -> (NumericFormat, NumericData)

instance RealClass Int8 where
  ndToReal tname (nf, nd) = do
    let fromType = numTypeName nd
        toType   = tname ?> "Int8"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumInt8   v -> return (nf, v)
      _           -> do
        let typePair = (fromType, toType)
        nes <- mapM (checkRange' typePair (0 :: Int8)) $ numToList nd
        return (nf, S.fromList $ map intCoerce nes)

  realToNd _ (nf, v) =
    let -- toType   = tname ?> "Int8"
        nd       = NumInt8 v
    -- checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    in (nf, nd)

instance RealClass Int16 where
  ndToReal tname (nf, nd) = do
    let fromType = numTypeName nd
        toType   = tname ?> "Int16"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumInt16  v -> return (nf, v)
      _           -> do
        let typePair = (fromType, toType)
        nes <- mapM (checkRange' typePair (0 :: Int16)) $ numToList nd
        return (nf, S.fromList $ map intCoerce nes)

  realToNd _ (nf, v) =
    let -- toType   = tname ?> "Int16"
        nd       = NumInt16 v
    -- checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    in (nf, nd)

instance RealClass Int32 where
  ndToReal tname (nf, nd) = do
    let fromType = numTypeName nd
        toType   = tname ?> "Int32"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumInt32  v -> return (nf, v)
      _           -> do
        let typePair = (fromType, toType)
        nes <- mapM (checkRange' typePair (0 :: Int32)) $ numToList nd
        return (nf, S.fromList $ map intCoerce nes)

  realToNd _ (nf, v) =
    let -- toType   = tname ?> "Int32"
        nd       = NumInt32 v
    -- checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    in (nf, nd)

instance RealClass Int64 where
  ndToReal tname (nf, nd) = do
    let fromType = numTypeName nd
        toType   = tname ?> "Int64"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumInt64  v -> return (nf, v)
      _           -> do
        let typePair = (fromType, toType)
        nes <- mapM (checkRange' typePair (0 :: Int64)) $ numToList nd
        return (nf, S.fromList $ map intCoerce nes)

  realToNd _ (nf, v) =
    let -- toType   = tname ?> "Int64"
        nd       = NumInt64 v
    -- checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    in (nf, nd)

instance RealClass Word8 where
  ndToReal tname (nf, nd) = do
    let fromType = numTypeName nd
        toType   = tname ?> "Word8"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumUnt8   v -> return (nf, v)
      _           -> do
        let typePair = (fromType, toType)
        nes <- mapM (checkRange' typePair (0 :: Word8)) $ numToList nd
        return (nf, S.fromList $ map intCoerce nes)

  realToNd _ (nf, v) =
    let -- toType   = tname ?> "Word8"
        nd       = NumUnt8 v
    -- checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    in (nf, nd)

instance RealClass Word16 where
  ndToReal tname (nf, nd) = do
    let fromType = numTypeName nd
        toType   = tname ?> "Word16"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumUnt16  v -> return (nf, v)
      _           -> do
        let typePair = (fromType, toType)
        nes <- mapM (checkRange' typePair (0 :: Word16)) $ numToList nd
        return (nf, S.fromList $ map intCoerce nes)

  realToNd _ (nf, v) =
    let -- toType   = tname ?> "Word16"
        nd       = NumUnt16 v
    -- checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    in (nf, nd)

instance RealClass Word32 where
  ndToReal tname (nf, nd) = do
    let fromType = numTypeName nd
        toType   = tname ?> "Word32"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumUnt32  v -> return (nf, v)
      _           -> do
        let typePair = (fromType, toType)
        nes <- mapM (checkRange' typePair (0 :: Word32)) $ numToList nd
        return (nf, S.fromList $ map intCoerce nes)

  realToNd _ (nf, v) =
    let -- toType   = tname ?> "Word32"
        nd       = NumUnt32 v
    -- checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    in (nf, nd)

instance RealClass Word64 where
  ndToReal tname (nf, nd) = do
    let fromType = numTypeName nd
        toType   = tname ?> "Word64"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumUnt64  v -> return (nf, v)
      _           -> do
        let typePair = (fromType, toType)
        nes <- mapM (checkRange' typePair (0 :: Word64)) $ numToList nd
        return (nf, S.fromList $ map intCoerce nes)

  realToNd _ (nf, v) =
    let -- toType   = tname ?> "Word64"
        nd       = NumUnt64 v
    -- checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    in (nf, nd)

instance RealClass Int where
  ndToReal _ = mapRight (second f) . ndToReal (Just "Int")
    where f :: S.Vector NativeInt -> S.Vector Int
          f = S.unsafeCast

  realToNd _ = realToNd (Just "Int") . second f
    where f :: S.Vector Int -> S.Vector NativeInt
          f = S.unsafeCast

instance RealClass Word where
  ndToReal _ = mapRight (second f) . ndToReal (Just "Word")
    where f :: S.Vector NativeWord -> S.Vector Word
          f = S.unsafeCast

  realToNd _ = realToNd (Just "Word") . second f
    where f :: S.Vector Word -> S.Vector NativeWord
          f = S.unsafeCast

instance RealClass Float where
  ndToReal tname (nf, nd) = do
    let -- fromType = numTypeName nd
        toType   = tname ?> "Float"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumFloat  v -> return (nf, v)
      _           -> do
        let nes = numToList nd
        return (nf, S.fromList $ map floatCoerce nes)

  realToNd _ (nf, v) =
    let -- toType   = tname ?> "Float"
        nd       = NumFloat v
    -- checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    in (nf, nd)

instance RealClass Double where
  ndToReal tname (nf, nd) = do
    let -- fromType = numTypeName nd
        toType   = tname ?> "Double"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumDouble v -> return (nf, v)
      _           -> do
        let nes = numToList nd
        return (nf, S.fromList $ map doubleCoerce nes)

  realToNd _ (nf, v) =
    let -- toType   = tname ?> "Double"
        nd       = NumDouble v
    -- checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    in (nf, nd)

class Storable a => ScalarClass a where
  ndToScalar :: Maybe String
             -> (NumericFormat, NumericData)
             -> Either PlasmaException (NumericFormat, S.Vector a)

  scalarToNd :: Maybe String
             -> (NumericFormat, S.Vector a)
             -> (NumericFormat, NumericData)
