{-|
Module      : Data.Slaw.Internal.Util
Description : Random utility functions that are useful
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

{-# LANGUAGE ScopedTypeVariables        #-}

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
  , (?>)
  , uncurry5
  , safeIntegralFromInteger
  , orList
  , eth2mby
  , tryIO
  ) where

import Control.Exception
import Data.Bits
-- import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char
import Data.Hashable
import Data.List
import Data.Word
import GHC.ByteOrder
import GHC.Float

infixl 0 ##
infix  7 ??
infix  4 ?>

{-# INLINE (##) #-}
-- | Synonym for 'hashWithSalt'.
(##) :: Hashable a => Int -> a -> Int
(##) = hashWithSalt

{-# INLINE (??) #-}
-- | Synonym for 'compare'.
(??) :: Ord a => a -> a -> Ordering
(??) = compare

-- | The byte order used by the platform we are running on.
nativeByteOrder :: ByteOrder
nativeByteOrder = targetByteOrder

{-# INLINABLE oppositeByteOrder #-}
-- | “Flips” the given byte order to the other one.
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

{-# INLINABLE (?>) #-}
-- | Returns the second argument if the first argument is 'Nothing'.
-- This makes it easy to provide a default value for an expression
-- that returns a 'Maybe'.
(?>) :: Maybe a -> a -> a
Just x  ?> _ = x
Nothing ?> x = x

uncurry5 :: (a -> b -> c -> d -> e -> f)
         -> (a, b, c, d, e)
         -> f
uncurry5 func (v, w, x, y, z) = func v w x y z

safeIntegralFromInteger :: forall a. (Integral a, Bits a)
                        => Integer
                        -> Either (Integer, Maybe Integer) a
safeIntegralFromInteger n =
  let signed = isSigned (undefined :: a)
  in case bitSizeMaybe (undefined :: a) of
    Nothing ->
      case (signed, signum n) of
        (False, (-1)) -> Left (0, Nothing)
        _             -> Right $ fromInteger n
    Just nbits ->
      let (lo, hi) = getLoHi nbits signed
      in if lo <= n && n <= hi
         then Right $ fromInteger n
         else Left (lo, Just hi)

getLoHi :: Int -> Bool -> (Integer, Integer)
getLoHi nbits False = (0, (2 ^ nbits) - 1)
getLoHi nbits True  = ((-x), x - 1)
  where x = 2 ^ (nbits - 1)

{-# INLINE orList #-}
orList :: Bits a => [a] -> a
orList = foldl' (.|.) zeroBits

{-# INLINABLE eth2mby #-}
eth2mby :: Either a b -> Maybe b
eth2mby (Left  _) = Nothing
eth2mby (Right x) = Just x

{-# INLINE tryIO #-}
-- | This handy utility function is just 'try', but constrained
-- to only work on 'IOException'.
tryIO :: IO a -> IO (Either IOException a)
tryIO = try
