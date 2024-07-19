{-# LANGUAGE CPP                        #-}

module Data.Slaw.Internal.OptionRecords
  ( WriteYamlOptions(..)
  , WriteBinaryOptions(..)
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
-- import Data.Slaw.Internal.Nameable
import Data.Slaw.Internal.OptionTypes
-- import Data.Slaw.Internal.SlawConvert
-- import Data.Slaw.Internal.SlawPath
-- import Data.Slaw.Internal.SlawType
-- import Data.Slaw.Internal.String
-- import Data.Slaw.Internal.Util

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

data WriteBinaryOptions = WriteBinaryOptions
  { wboByteOrder :: !PreferredByteOrder
  , wboAutoFlush :: !AutoFlush
  } deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

instance Default WriteBinaryOptions where
  def = WriteBinaryOptions
        { wboByteOrder = def
        , wboAutoFlush = def
        }
