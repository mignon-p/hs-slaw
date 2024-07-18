module Data.Slaw.Internal.OptionTypes
  ( PreferredByteOrder(..)
  , AutoFlush(..)
  , StrNumNone(..)
  ) where

import Control.DeepSeq
import Data.Default.Class
import Data.Hashable
-- import Data.Int
import qualified Data.Text                as T
-- import Data.Word
import GHC.Generics (Generic)
import Numeric.Natural

-- import Data.Slaw.Internal.SlawConvert
-- import Data.Slaw.Internal.SlawType
-- import Data.Slaw.Internal.Util

data PreferredByteOrder = BoNative
                        | BoLittleEndian
                        | BoBigEndian
                        deriving (Eq, Ord, Show, Read, Bounded, Enum,
                                  Generic, NFData, Hashable)

instance Default PreferredByteOrder where
  def = BoNative

data AutoFlush = AutoFlushNever
               | AutoFlushAlways
               | AutoFlushIfNotSeekable
               deriving (Eq, Ord, Show, Read, Bounded, Enum,
                         Generic, NFData, Hashable)

instance Default AutoFlush where
  def = AutoFlushIfNotSeekable

data StrNumNone = StringValue  !T.Text
                | NumericValue !Natural
                | NoValue
                deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

instance Default StrNumNone where
  def = NoValue
