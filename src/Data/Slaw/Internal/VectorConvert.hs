{-# LANGUAGE ScopedTypeVariables        #-}

module Data.Slaw.Internal.VectorConvert
  ( ByteOrder(..) -- re-export
  , nativeByteOrder
  , vToBs
  ) where

import Control.Monad
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as B
import Data.Int
import qualified Data.Vector.Storable     as S
import Data.Word
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.ByteOrder
import System.IO.Unsafe

nativeByteOrder :: ByteOrder
nativeByteOrder = targetByteOrder

class Storable a => Swappable a where
  getSwapFunc :: a -> Maybe (Ptr () -> Int -> IO ())

instance Swappable Word8 where
  getSwapFunc _ = Nothing

instance Swappable Word16 where
  getSwapFunc _ = Just swapArray16

instance Swappable Word32 where
  getSwapFunc _ = Just swapArray32

instance Swappable Word64 where
  getSwapFunc _ = Just swapArray64

instance Swappable Int8 where
  getSwapFunc _ = Nothing

instance Swappable Int16 where
  getSwapFunc _ = Just swapArray16

instance Swappable Int32 where
  getSwapFunc _ = Just swapArray32

instance Swappable Int64 where
  getSwapFunc _ = Just swapArray64

swapArray16 :: Ptr () -> Int -> IO ()
swapArray16 vptr len = do
  let p = castPtr vptr :: Ptr Word16
  forM_ [0..len-1] $ \i -> do
    x <- peekElemOff p i
    pokeElemOff p i (byteSwap16 x)

swapArray32 :: Ptr () -> Int -> IO ()
swapArray32 vptr len = do
  let p = castPtr vptr :: Ptr Word32
  forM_ [0..len-1] $ \i -> do
    x <- peekElemOff p i
    pokeElemOff p i (byteSwap32 x)

swapArray64 :: Ptr () -> Int -> IO ()
swapArray64 vptr len = do
  let p = castPtr vptr :: Ptr Word64
  forM_ [0..len-1] $ \i -> do
    x <- peekElemOff p i
    pokeElemOff p i (byteSwap64 x)

isAligned :: forall a. Storable a => Ptr a -> Bool
isAligned p = p == p'
  where p' = alignPtr p $ alignment (undefined :: a)

vToBs :: forall a. Swappable a => ByteOrder -> S.Vector a -> B.ByteString
vToBs bo v = B.BS (castForeignPtr bsPtr) bsLen
  where
    size     = sizeOf (undefined :: a)
    swapFunc = if bo == nativeByteOrder
               then Nothing
               else getSwapFunc (undefined :: a)
    (vPtr, vLen) = S.unsafeToForeignPtr0 v
    vPtr'    = castForeignPtr vPtr
    bsPtr    = case swapFunc of
                 Nothing -> vPtr'
                 Just sf ->
                   unsafePerformIO $ copyAndSwap sf size vPtr' vLen
    bsLen    = vLen * size

copyAndSwap :: (Ptr () -> Int -> IO ())
            -> Int
            -> ForeignPtr ()
            -> Int
            -> IO (ForeignPtr ())
copyAndSwap sf elemSize oldPtr nElems = do
  let nBytes = elemSize * nElems
  newPtr <- mallocForeignPtrBytes nBytes
  withForeignPtr newPtr $ \newP -> do
    withForeignPtr oldPtr $ \oldP -> do
      copyBytes newP oldP nBytes
    sf newP nElems
  return newPtr
