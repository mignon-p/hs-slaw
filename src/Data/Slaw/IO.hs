{-|
Module      : Data.Slaw.IO
Description :
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module Data.Slaw.IO
  ( SlawInputStream
  , openBinarySlawInput
  , siName
  , siRead
  , siClose
  , readBinarySlawFile
  , SlawOutputStream
  , openBinarySlawOutput
  , soName
  , soWrite
  , soFlush
  , soClose
  , writeBinarySlawFile
  , WriteBinaryOptions(..)
  , PreferredByteOrder(..)
  , bo2pbo
  , pbo2bo
  , AutoFlush(..)
  , FileClass
  , NoClose(..)
  ) where

import Data.Slaw.Internal.FileClass
import Data.Slaw.Internal.OptionRecords
import Data.Slaw.Internal.OptionTypes
import Data.Slaw.Internal.SlawIO
