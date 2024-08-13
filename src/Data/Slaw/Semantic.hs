{-|
Module      : Data.Slaw.Semantic
Description : Compare slawx semantically, rather than literally
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC

'Semantic' wraps a 'Slaw', but its 'Eq', 'Ord', and 'Hashable'
instances behave differently.

'SlawNumeric' is treated as equal if the 'NumericFormat' is equal,
and the 'NumericData' represents the same numbers mathematically,
regardless of their bit width and whether they are signed, unsigned,
or floating point.

'SlawError' is treated as equal to any other 'SlawError', regardless
of the error message or location.

'SlawMap' is treated as equal to another 'SlawMap' if the key/value
pairs are equal, regardless of the order they appear in.

In 'SlawProtein', if the descrips is 'Nothing', it is treated the
same as an empty list.  If the ingests is 'Nothing', it is treated
the same as an empty map.
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
