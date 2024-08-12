{-|
Module      : Data.Slaw.Internal.HalfOrphans
Description : Orphan instances for Numeric.Half
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module Data.Slaw.Internal.HalfOrphans () where

import Data.Hashable
import Data.Word
import Numeric.Half
import Text.Printf

import Data.Slaw.Internal.Util

instance Hashable Half where
  salt `hashWithSalt` half = salt ## (w16 :: Word16)
    where w16 = if | isZero half -> 0
                   | isNaN  half -> 0x7fff
                   | otherwise   -> (fromIntegral . getHalf) half

instance PrintfArg Half where
  formatArg = formatRealFloat
