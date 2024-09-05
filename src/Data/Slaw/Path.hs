{-|
Module      : Data.Slaw.Path
Description : Extract data from deeply-nested slawx with a query string
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC

These functions make it easy to extract a particular 'Slaw' from
a deeply nested 'Slaw' structure.  The location is specified with
a string that contains components separated by slashes, similar to
a pathname on the filesystem.  Each component goes one level deeper
in the nested 'Slaw' structure, like traversing directories on a
filesystem.

    * If the 'Slaw' is a 'SlawMap', the component specifies the key
      to look up in the map.  The keys are normalized to @NFKD@
      before comparing, and may also be case-folded, depending on
      the value of 'spoCaseInsensitive'.

    * If the 'Slaw' is a 'SlawList', then the component must be a
      decimal integer, which specifies the (0-based) index to
      look up in the list.

    * If the 'Slaw' is a 'SlawCons', then the component must be
      either @car@ (which can be abbreviated @a@ or @0@), or
      @cdr@ (which can be abbreviated @d@ or @1@).

    * If the 'Slaw' is a 'SlawProtein', the behavior depends on the
      'ProteinMode'.

        * If 'ProteinMode' is 'PmUseIngests', then resolution skips
          directly to the ingests of the protein, without consuming
          a component from the path.

        * If 'ProteinMode' is 'PmFullyVisible', then the component
          must be one of:

            * @descrips@ (abbreviated @d@, @des@, or @0@)
            * @ingests@ (abbreviated @i@, @ing@, or @1@)
            * @rudeData@ (abbreviated @r@, @rude@, or @2@)

    * If the 'Slaw' is a 'SlawNumeric', then it may be treated as
      zero or more levels, depending on its 'NumericFormat'.

        * If 'nfArray' is 'True', then the next component must be a
          decimal integer, which specifies the (0-based) index
          within the array.

        * If 'nfVector' is not equal to 'VtScalar', then the next
          component must be either a decimal integer index into
          the vector (from 0 to at most 3, depending on the
          dimensionality of the vector), or one of the four
          letters @xyzw@ (which correspond to the indices @0123@).

        * If 'nfComplex' is 'True', then the next component must
          be one of:

            * @real@ (abbreviated @r@, @re@, or @0@)
            * @imaginary@ (abbreviated @i@, @im@, or @1@)
-}

{-# OPTIONS_GHC -Wno-unused-imports     #-}

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
    --
    -- | These use the default 'SlawPathOpts'.  ('PmUseIngests',
    -- and case-insensitive comparison.)
  , (!)
  , (!?)
  ) where

import Data.Slaw ( Slaw(..)
                 , NumericFormat(..)
                 , NumericData(..)
                 , VectorType(..)
                 )
import Data.Slaw.Internal.SlawPath
import Data.Slaw.Internal.Util
