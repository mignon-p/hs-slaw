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
  , forLeft
  , forRight
  , (==~)
  , (?>)
  , uncurry5
  ) where

import Data.Bits
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char
import Data.Hashable
import Data.Word
import GHC.ByteOrder
import GHC.Float

infixl 0 ##
infix  7 ??
infix  4 ==~
infix  4 ?>

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

{-# INLINABLE ucFirst #-}
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

{-# INLINE forLeft #-}
forLeft :: Either a b -> (a -> c) -> Either c b
forLeft = flip mapLeft

{-# INLINE forRight #-}
forRight :: Either a b -> (b -> c) -> Either a c
forRight = flip mapRight

-- case insensitive (for ASCII chars only) equality for lazy ByteStrings
{-# INLINABLE (==~) #-}
(==~) :: L8.ByteString -> L8.ByteString -> Bool
x ==~ y
  | L8.length x /= L8.length y = False
  | otherwise = L8.map lcAscii x == L8.map lcAscii y

lcAscii :: Char -> Char
lcAscii c
  | n >= 0x41 && n <= 0x5A = chr (n + 0x20)
  | otherwise              = c
  where n = ord c

{-# INLINABLE (?>) #-}
(?>) :: Maybe a -> a -> a
Just x  ?> _ = x
Nothing ?> x = x

uncurry5 :: (a -> b -> c -> d -> e -> f)
         -> (a, b, c, d, e)
         -> f
uncurry5 func (v, w, x, y, z) = func v w x y z
