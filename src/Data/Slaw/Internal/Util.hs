module Data.Slaw.Internal.Util
  ( (##)
  , (??)
  , float2Double -- re-export
  , double2Float -- re-export
  ) where

import Data.Hashable
import GHC.Float

infixl 0 ##
infix  7 ??

{-# INLINE (##) #-}
(##) :: Hashable a => Int -> a -> Int
(##) = hashWithSalt

{-# INLINE (??) #-}
(??) :: Ord a => a -> a -> Ordering
(??) = compare
