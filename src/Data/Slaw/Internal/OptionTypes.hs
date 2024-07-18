{-# LANGUAGE ScopedTypeVariables        #-}

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
import qualified Data.Vector.Storable     as S
-- import Data.Word
import Foreign.Storable
import GHC.Generics (Generic)
import Numeric.Natural

import Data.Slaw.Internal.Nameable
import Data.Slaw.Internal.SlawConvert
import Data.Slaw.Internal.SlawType
import Data.Slaw.Internal.String
-- import Data.Slaw.Internal.Util

data PreferredByteOrder = BoNative
                        | BoLittleEndian
                        | BoBigEndian
                        deriving (Eq, Ord, Show, Read, Bounded, Enum,
                                  Generic, NFData, Hashable)

instance Default PreferredByteOrder where
  def = BoNative

instance Nameable PreferredByteOrder where
  typeName _ =   "PreferredByteOrder"

data AutoFlush = AutoFlushNever
               | AutoFlushAlways
               | AutoFlushIfNotSeekable
               deriving (Eq, Ord, Show, Read, Bounded, Enum,
                         Generic, NFData, Hashable)

instance Default AutoFlush where
  def = AutoFlushIfNotSeekable

instance Nameable AutoFlush where
  typeName _ =   "AutoFlush"

data StrNumNone = StringValue  !T.Text
                | NumericValue !Natural
                | NoValue
                deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

instance Default StrNumNone where
  def = NoValue

instance Nameable StrNumNone where
  typeName _ =   "StrNumNone"

instance FromSlaw StrNumNone where
  fromSlaw (SlawString    utf8) = (return . StringValue . fromUtf8) utf8
  fromSlaw s@(SlawNumeric _ _ ) = do
    n <- fromSlaw s
    if n < 0
      then return NoValue
      else NumericValue <$> fromSlaw (fromInteger n)
  fromSlaw SlawNil              = return NoValue
  fromSlaw s                    = handleOthers s

instance ToSlaw StrNumNone where
  toSlaw NoValue           = preferNumeric NumInt32 (-1)
  toSlaw (NumericValue n)  = preferNumeric NumInt32 $ toInteger n
  toSlaw (StringValue txt) = SlawString $ toUtf8 txt

--

preferNumeric :: forall a. (Integral a, Bounded a, Storable a)
              => (S.Vector a -> NumericData)
              -> Integer
              -> Slaw
preferNumeric con n =
  let lo = toInteger (minBound :: a)
      hi = toInteger (maxBound :: a)
  in if n >= lo && n <= hi
     then SlawNumeric def $ con $ S.singleton $ fromInteger n
     else toSlaw n
