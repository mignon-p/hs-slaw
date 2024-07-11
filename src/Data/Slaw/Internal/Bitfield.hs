module Data.Slaw.Internal.Bitfield
  ( Bitfield(..)
  , getBf
  , setBf
  , getBf'
  , setBf'
  , getBfBool
  , setBfBool
  , apply
  ) where

import Control.DeepSeq
import Data.Bits
import Data.Hashable
import GHC.Generics (Generic)

data Bitfield = Bitfield
  { bfPos   :: {-# UNPACK #-} !Int
  , bfWidth :: {-# UNPACK #-} !Int
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)

{-# INLINE getBf #-}
getBf :: (Bits a, Integral a) => Bitfield -> a -> a
getBf (Bitfield pos w) !val = (val `shiftR` pos) .&. mkMask w

{-# INLINE setBf #-}
setBf :: (Bits a, Integral a) => Bitfield -> a -> a -> a
setBf (Bitfield pos w) !x !val =
  let mask  = mkMask w
      x'    = x .&. mask
      val'  = val .&. complement (mask `shiftL` pos)
  in val' .|. (x' `shiftL` pos)

{-# INLINE getBf' #-}
getBf' :: (Bits a, Integral a, Integral b) => Bitfield -> a -> b
getBf' bf = fromIntegral . getBf bf

{-# INLINE setBf' #-}
setBf' :: (Bits a, Integral a, Integral b) => Bitfield -> b -> a -> a
setBf' bf x = setBf bf (fromIntegral x)

{-# INLINE getBfBool #-}
getBfBool :: Bits a => Bitfield -> a -> Bool
getBfBool (Bitfield pos _) !val = val `testBit` pos

{-# INLINE setBfBool #-}
setBfBool :: Bits a => Bitfield -> Bool -> a -> a
setBfBool (Bitfield pos _) True  !val = val `setBit`   pos
setBfBool (Bitfield pos _) False !val = val `clearBit` pos

{-# INLINE mkMask #-}
mkMask :: (Bits a, Integral a) => Int -> a
mkMask !w = bit w - 1

{-# INLINABLE apply' #-}
apply' :: a -> [a -> a] -> a
apply' !val []          = val
apply' !val (func:rest) = apply' (func val) rest

{-# INLINE apply #-}
apply :: Num a => [a -> a] -> a
apply = apply' 0
