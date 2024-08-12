{-|
Module      : Data.Slaw.Semantic
Description :
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module Data.Slaw.Semantic
  ( Semantic(..)
  , unSemantic
  , SemanticCI(..)
  , unSemanticCI
  , (==~)
  , (==~~)
  ) where

import Data.Slaw.Internal.SemanticSlaw
