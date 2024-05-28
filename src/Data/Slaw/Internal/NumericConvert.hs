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

{-
cnfScalar :: CheckNF
cnfScalar = CheckNF
  { cnfArray   = Nothing
  , cnfComplex = Nothing
  , cnfVector  = Just VtScalar
  }

insertZeros :: Storable a => S.Vector a -> S.Vector a
insertZeros = undefined
-}

class Storable a => ScalarClass a where
  ndToScalar :: Maybe String
             -> (NumericFormat, NumericData)
             -> Either PlasmaException (S.Vector a)

  scalarToNd :: S.Vector a
             -> (NumericFormat, NumericData)

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
             Right nd0 -> return nd0
    let nd2 = if nfComplex nf
              then nd1
              else insertZeros nd1
    undefined nd2 -- TODO: cast Vector a to Vector (Complex a)

  scalarToNd = undefined
  scalarName = undefined
-}
