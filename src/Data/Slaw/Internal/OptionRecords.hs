{-|
Module      : Data.Slaw.Internal.OptionRecords
Description : Record types that represent slaw option maps
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

{-# LANGUAGE CPP                        #-}

module Data.Slaw.Internal.OptionRecords
  ( WriteBinaryOptions(..)
  ) where

import Control.DeepSeq
import Data.Default.Class
import Data.Hashable
import GHC.Generics (Generic)

import Data.Slaw.Internal.Nameable
import Data.Slaw.Internal.OptionTypes
import Data.Slaw.Internal.SlawConvert
-- import Data.Slaw.Internal.SlawType

#define FIELD(qzName, qzField) opt qzName qzField \
  (\qzRec qzVal -> qzRec { qzField = qzVal })

#define NFELD(qzName, qzField, qzPref) optN qzName qzField \
  (\qzRec qzVal -> qzRec { qzField = qzVal }) qzPref

--

-- | Record which can be passed as the second argument to
-- 'Data.Slaw.IO.openBinarySlawOutput' or
-- 'Data.Slaw.IO.writeBinarySlawFile', to specify options.
data WriteBinaryOptions = WriteBinaryOptions
  { -- | Byte order to write the binary file in.  Doesn't really
    -- matter, because it will be automatically swapped on read if
    -- necessary.
    --
    -- [key]: @byte-order@
    --
    -- [type]: string - one of @little-endian@, @big-endian@, or @native@.
    --
    -- [default]: @native@
    wboByteOrder :: Maybe PreferredByteOrder
    -- | Whether to automatically flush the stream whenever a slaw
    -- is written.  Defaults to only do so if the stream is not
    -- seekable (and therefore likely a pipe, socket, or terminal).
    --
    -- [key]: @auto-flush@
    --
    -- [type]: string - one of @never@, @always@, or @if-not-seekable@.
    --
    -- [default]: @if-not-seekable@
  , wboAutoFlush :: Maybe AutoFlush
} deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

instance Default WriteBinaryOptions where
  def = WriteBinaryOptions
        { wboByteOrder = Nothing
        , wboAutoFlush = Nothing
        }

instance Nameable WriteBinaryOptions where
  typeName _ = "WriteBinaryOptions"

instance FromSlaw WriteBinaryOptions where
  fromSlaw = Right . recordFromMap writeBinaryOptions

instance ToSlaw WriteBinaryOptions where
  toSlaw = recordToMapWithFmt writeBinaryOptions BinaryFile

writeBinaryOptions :: Options WriteBinaryOptions
writeBinaryOptions =
  [ FIELD("byte-order", wboByteOrder)
  , FIELD("auto-flush", wboAutoFlush)
  ]
