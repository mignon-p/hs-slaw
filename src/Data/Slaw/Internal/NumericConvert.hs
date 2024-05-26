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

  realToNd :: (NumericFormat, S.Vector a)
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

  realToNd = second NumInt8

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

  realToNd = second NumInt16

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

  realToNd = second NumInt32

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

  realToNd = second NumInt64

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

  realToNd = second NumUnt8

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

  realToNd = second NumUnt16

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

  realToNd = second NumUnt32

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

  realToNd = second NumUnt64

instance RealClass Int where
  ndToReal _ = mapRight (second f) . ndToReal (Just "Int")
    where f :: S.Vector NativeInt -> S.Vector Int
          f = S.unsafeCast

  realToNd = realToNd . second f
    where f :: S.Vector Int -> S.Vector NativeInt
          f = S.unsafeCast

instance RealClass Word where
  ndToReal _ = mapRight (second f) . ndToReal (Just "Word")
    where f :: S.Vector NativeWord -> S.Vector Word
          f = S.unsafeCast

  realToNd = realToNd . second f
    where f :: S.Vector Word -> S.Vector NativeWord
          f = S.unsafeCast

instance RealClass Float where
  ndToReal tname (nf, nd) = do
    let toType   = tname ?> "Float"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumFloat  v -> return (nf, v)
      _           -> do
        let nes = numToList nd
        return (nf, S.fromList $ map floatCoerce nes)

  realToNd = second NumFloat

instance RealClass Double where
  ndToReal tname (nf, nd) = do
    let toType   = tname ?> "Double"
    checkNF cnfReal nf (nf { nfArray = False }, nd, toType)
    case nd of
      NumDouble v -> return (nf, v)
      _           -> do
        let nes = numToList nd
        return (nf, S.fromList $ map doubleCoerce nes)

  realToNd = second NumDouble

class Storable a => ScalarClass a where
  ndToScalar :: Maybe String
             -> (NumericFormat, NumericData)
             -> Either PlasmaException (NumericFormat, S.Vector a)

  scalarToNd :: (NumericFormat, S.Vector a)
             -> (NumericFormat, NumericData)
