{-|
Module      : Data.Slaw.Semantic
Description : Compare slawx semantically, rather than literally
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module Data.Slaw.Semantic
  ( -- * Semantic comparison
    Semantic(..)
  , unSemantic
  , (==~)
    -- * Case-insensitive semantic comparison
  , SemanticCI(..)
  , unSemanticCI
  , (==~~)
  ) where

import Data.Slaw.Internal.SemanticSlaw
