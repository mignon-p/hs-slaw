{-|
Module      : Data.Slaw.Path
Description : Extract data from deeply-nested slawx with a query string
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module Data.Slaw.Path
  ( -- * Options
    ProteinMode(..)
  , SlawPathOpts(..)
    -- * Functions
  , slawPath
  , slawPath_m
  , slawPath_es
  , slawPath_ee
    -- * Operators
  , (!)
  , (!?)
  , (?>)
  ) where

import Data.Slaw.Internal.SlawPath
import Data.Slaw.Internal.Util
