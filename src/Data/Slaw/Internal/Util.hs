module Data.Slaw.Internal.Util
  ( (##)
  , (??)
  , ByteOrder(..) -- re-export
  , nativeByteOrder
  , oppositeByteOrder
  , float2Double -- re-export
  , double2Float -- re-export
  ) where

import Data.Hashable
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
