module Data.Slaw.Internal.Util
  ( (##)
  , (??)
  , ByteOrder(..) -- re-export
  , nativeByteOrder
  , oppositeByteOrder
  , float2Double -- re-export
  , double2Float -- re-export
  , ucFirst
  , lo56
  , hi8
  , mapLeft
  , mapRight
  ) where

import Data.Bits
import Data.Char
import Data.Hashable
import Data.Word
import GHC.ByteOrder
import GHC.Float

infixl 0 ##
infix  7 ??

{-# INLINE (##) #-}
(##) :: Hashable a => Int -> a -> Int
(##) = hashWithSalt

{-# INLINE (??) #-}
(??) :: Ord a => a -> a -> Ordering
(??) = compare

nativeByteOrder :: ByteOrder
nativeByteOrder = targetByteOrder

{-# INLINABLE oppositeByteOrder #-}
oppositeByteOrder :: ByteOrder -> ByteOrder
oppositeByteOrder BigEndian    = LittleEndian
oppositeByteOrder LittleEndian = BigEndian

ucFirst :: String -> String
ucFirst [] = []
ucFirst (x:rest) = toUpper x : rest

-- least significant 7 bytes of an oct
{-# INLINE lo56 #-}
lo56 :: Word64 -> Word64
lo56 = (.&. complement 0xff00_0000_0000_0000)

-- most significant byte of an oct
{-# INLINE hi8 #-}
hi8 :: Word64 -> Word64
hi8 = (`shiftR` 56)

{-# INLINABLE mapLeft #-}
mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x)  = Left (f x)
mapLeft _ (Right x) = Right x

{-# INLINABLE mapRight #-}
mapRight :: (b -> c) -> Either a b -> Either a c
mapRight _ (Left x)  = Left  x
mapRight f (Right x) = Right (f x)
