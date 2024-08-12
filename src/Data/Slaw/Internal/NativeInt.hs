{-|
Module      : Data.Slaw.Internal.NativeInt
Description : Treating Int as either Int32 or Int64, depending on CPU
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

{-# LANGUAGE CPP                        #-}

module Data.Slaw.Internal.NativeInt
  ( NativeWord
  , NativeInt
  , toNativeWord
  , fromNativeWord
  , toNativeInt
  , fromNativeInt
  ) where

import Data.Int
import Data.Word

#include "MachDeps.h"

#if WORD_SIZE_IN_BITS > 32
type NativeWord = Word64
type NativeInt  = Int64
#else
type NativeWord = Word32
type NativeInt  = Int32
#endif

{-# INLINE toNativeWord #-}
toNativeWord :: Integral a => a -> NativeWord
toNativeWord = fromIntegral

{-# INLINE fromNativeWord #-}
fromNativeWord :: Integral a => NativeWord -> a
fromNativeWord = fromIntegral

{-# INLINE toNativeInt #-}
toNativeInt :: Integral a => a -> NativeInt
toNativeInt = fromIntegral

{-# INLINE fromNativeInt #-}
fromNativeInt :: Integral a => NativeInt -> a
fromNativeInt = fromIntegral
