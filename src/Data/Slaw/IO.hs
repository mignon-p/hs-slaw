{-|
Module      : Data.Slaw.IO
Description : Read and write binary slaw files
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module Data.Slaw.IO
  ( -- * Reading slawx from a file
    SlawInputStream
  , openBinarySlawInput
  , siName
  , siRead
  , siClose
  , readBinarySlawFile
    -- * Writing slawx from a file
  , SlawOutputStream
  , openBinarySlawOutput
  , soName
  , soWrite
  , soFlush
  , soClose
  , writeBinarySlawFile
    -- ** Options for writing slawx to a binary file
  , WriteBinaryOptions(..)
  , PreferredByteOrder(..)
  , bo2pbo
  , pbo2bo
  , AutoFlush(..)
    -- * File-related types
  , FileClass
  , NoClose(..)
  ) where

import Data.Slaw.Internal.FileClass
import Data.Slaw.Internal.OptionRecords
import Data.Slaw.Internal.OptionTypes
import Data.Slaw.Internal.SlawIO
