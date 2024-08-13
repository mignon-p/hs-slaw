{-|
Module      : Data.Slaw.IO
Description : Read and write binary slaw files
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC

These functions read and write binary slaw files, which consist of
an eight-byte header, followed by zero or more slawx.  The header
includes a magic number, and also the endianness which the slawx
were written in.  Endianness is converted automatically when reading.

File names are represented by the typeclass 'FileClass', which can
be any of:

    * 'FilePath' (i. e. 'String')
    * 'System.OsPath.OsPath'
    * 'Handle', which is closed when the 'SlawInputStream' or
      'SlawOutputStream' is closed
    * 'NoClose', a @newtype@ which wraps a 'Handle', and leaves the
      handle open when the 'SlawInputStream' or 'SlawOutputStream'
      is closed
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
