{-# LANGUAGE CPP                        #-}

module Data.Slaw.Internal.OptionRecords
  ( WriteYamlOptions(..)
  , WriteBinaryOptions(..)
  , WriteFileOptions(..)
  ) where

import Control.DeepSeq
-- import qualified Data.ByteString.Short    as SBS
import Data.Default.Class
import Data.Hashable
-- import Data.Int
-- import Data.Maybe
-- import qualified Data.Text                as T
-- import qualified Data.Vector.Storable     as S
-- import Data.Word
-- import Foreign.Storable
import GHC.Generics (Generic)
import Numeric.Natural

-- import Data.Slaw.Internal.EnumStrings
-- import Data.Slaw.Internal.Exception
import Data.Slaw.Internal.Nameable
import Data.Slaw.Internal.OptionTypes
import Data.Slaw.Internal.SlawConvert
-- import Data.Slaw.Internal.SlawPath
import Data.Slaw.Internal.SlawType
-- import Data.Slaw.Internal.String
-- import Data.Slaw.Internal.Util

#define FIELD(qzName, qzField) opt qzName qzField \
  (\qzRec qzVal -> qzRec { qzField = qzVal })

#define CFELD(qzName, qzField, qzTo, qzFrom) opt1 qzName qzField \
  (\qzRec qzVal -> qzRec { qzField = qzVal }) qzTo qzFrom

--

data WriteYamlOptions = WriteYamlOptions
  { wyoTagNumbers       :: !Bool
  , wyoDirectives       :: !Bool
  , wyoOrderedMaps      :: !Bool
  , wyoComment          :: !Bool
  , wyoMaxArrayElements :: !(Maybe Natural)
  , wyoAutoFlush        :: !AutoFlush
  } deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

instance Default WriteYamlOptions where
  def = WriteYamlOptions
        { wyoTagNumbers       = True
        , wyoDirectives       = True
        , wyoOrderedMaps      = True
        , wyoComment          = False
        , wyoMaxArrayElements = Nothing
        , wyoAutoFlush        = def
        }

instance Nameable WriteYamlOptions where
  typeName _ = "WriteYamlOptions"

instance FromSlaw WriteYamlOptions where
  fromSlaw = Right . recordFromMap writeYamlOptions

instance ToSlaw WriteYamlOptions where
  toSlaw wyo = SlawMap (p1 ++ p2)
    where
      p1 = recordToPairs writeFileOptions $ WriteFileOptions YamlFile
      p2 = recordToPairs writeYamlOptions wyo

writeYamlOptions :: Options WriteYamlOptions
writeYamlOptions =
  [ FIELD("tag_numbers",        wyoTagNumbers)
  , FIELD("directives",         wyoDirectives)
  , FIELD("ordered_maps",       wyoOrderedMaps)
  , FIELD("comment",            wyoComment)
  , CFELD("max-array-elements", wyoMaxArrayElements, tm64, fm64)
  , FIELD("auto-flush",         wyoAutoFlush)
  ]

tm64 :: Maybe Natural -> Slaw
tm64 Nothing  = preferNumeric NumInt64 (-1)
tm64 (Just n) = preferNumeric NumInt64 $ toInteger n

fm64 :: Slaw -> Maybe (Maybe Natural)
fm64 SlawNil = return Nothing
fm64 s       = do
  n <- ŝm s
  if n < 0
    then return Nothing
    else return $ Just $ fromInteger n

--

data WriteBinaryOptions = WriteBinaryOptions
  { wboByteOrder :: !PreferredByteOrder
  , wboAutoFlush :: !AutoFlush
  } deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

instance Default WriteBinaryOptions where
  def = WriteBinaryOptions
        { wboByteOrder = def
        , wboAutoFlush = def
        }

instance Nameable WriteBinaryOptions where
  typeName _ = "WriteBinaryOptions"

instance FromSlaw WriteBinaryOptions where
  fromSlaw = Right . recordFromMap writeBinaryOptions

instance ToSlaw WriteBinaryOptions where
  toSlaw wbo = SlawMap (p1 ++ p2)
    where
      p1 = recordToPairs writeFileOptions $ WriteFileOptions BinaryFile
      p2 = recordToPairs writeBinaryOptions wbo

writeBinaryOptions :: Options WriteBinaryOptions
writeBinaryOptions =
  [ FIELD("byte-order", wboByteOrder)
  , FIELD("auto-flush", wboAutoFlush)
  ]

--

data WriteFileOptions = WriteFileOptions
  { wfoFileFormat :: !FileFormat
  } deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

instance Default WriteFileOptions where
  def = WriteFileOptions def

instance Nameable WriteFileOptions where
  typeName _ = "WriteFileOptions"

instance FromSlaw WriteFileOptions where
  fromSlaw = Right . recordFromMap writeFileOptions

instance ToSlaw WriteFileOptions where
  toSlaw = recordToMap writeFileOptions

writeFileOptions :: Options WriteFileOptions
writeFileOptions =
  [ FIELD("format", wfoFileFormat)
  ]
