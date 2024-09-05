{-|
Module      : Data.Slaw.Util
Description : Some handy stuff that's not directly related to slaw.
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module Data.Slaw.Util
  ( -- * Typeclasses for strings and bytestrings
    TextClass(..)
  , ByteStringClass(..)
    -- * Handy operators
  , (##)
  , (??)
  , (?>)
  ) where

import Data.Slaw.Internal.String
import Data.Slaw.Internal.Util
