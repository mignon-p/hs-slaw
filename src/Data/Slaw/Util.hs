{-|
Module      : Data.Slaw.Util
Description : Some handy stuff that's not directly related to slaw.
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC

Some handy stuff that's not directly related to slaw.
-}

module Data.Slaw.Util
  ( -- * Useful typeclasses
    TextClass(..)
  , ByteStringClass(..)
  , Merge(..)
    -- * Handy operators
  , (##)
  , (??)
  , (?>)
  ) where

import Data.Slaw.Internal.Merge
import Data.Slaw.Internal.String
import Data.Slaw.Internal.Util
