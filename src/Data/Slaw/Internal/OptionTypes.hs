{-|
Module      : Data.Slaw.Internal.OptionTypes
Description : Types that represent options
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

{-# LANGUAGE ScopedTypeVariables        #-}

module Data.Slaw.Internal.OptionTypes
  ( PreferredByteOrder(..)
  , bo2pbo
  , pbo2bo
  , AutoFlush(..)
  , StrOrInt(..)
  , FileFormat(..)
  --
  , preferNumeric
  , Option
  , Options
  , opt
  , optN
  , opt1
  , recordFromMap
  , recordFromMap0
  , recordToMap
  , recordToMapWithFmt
  , recordToPairs
  , coerceToMap
  , kFormat
  ) where

import Control.DeepSeq
import Data.Bifunctor
import qualified Data.ByteString.Short    as SBS
import Data.Default.Class
import Data.Hashable
-- import Data.Int
import Data.Maybe
import qualified Data.Text                as T
import qualified Data.Vector.Storable     as S
-- import Data.Word
import Foreign.Storable
import GHC.Generics (Generic)
-- import Numeric.Natural

import Data.Slaw.Internal.EnumStrings
import Data.Slaw.Internal.Exception
import Data.Slaw.Internal.Nameable
import Data.Slaw.Internal.SlawConvert
import Data.Slaw.Internal.SlawPath
import Data.Slaw.Internal.SlawType
import Data.Slaw.Internal.String
import Data.Slaw.Internal.Util

data PreferredByteOrder = BoNative
                        | BoLittleEndian
                        | BoBigEndian
                        deriving (Eq, Ord, Show, Read, Bounded, Enum,
                                  Generic, NFData, Hashable)

instance Default PreferredByteOrder where
  def = BoNative

instance Nameable PreferredByteOrder where
  typeName _ =   "PreferredByteOrder"

instance FromSlaw PreferredByteOrder where
  fromSlaw = enumFromSlaw pboStrs

instance ToSlaw PreferredByteOrder where
  toSlaw = enumToSlaw pboStrs

pboStrs :: EnumStrings PreferredByteOrder
pboStrs = makeEnumStrings
  [ ("native n",                            BoNative      )
  , ("little-endian LittleEndian little l", BoLittleEndian)
  , ("big-endian    BigEndian    big    b", BoBigEndian   )
  ]

bo2pbo :: ByteOrder -> PreferredByteOrder
bo2pbo BigEndian    = BoBigEndian
bo2pbo LittleEndian = BoLittleEndian

pbo2bo :: PreferredByteOrder -> ByteOrder
pbo2bo BoBigEndian    = BigEndian
pbo2bo BoLittleEndian = LittleEndian
pbo2bo BoNative       = nativeByteOrder

--

data AutoFlush = AutoFlushNever
               | AutoFlushAlways
               | AutoFlushIfNotSeekable
               deriving (Eq, Ord, Show, Read, Bounded, Enum,
                         Generic, NFData, Hashable)

instance Default AutoFlush where
  def = AutoFlushIfNotSeekable

instance Nameable AutoFlush where
  typeName _ =   "AutoFlush"

instance FromSlaw AutoFlush where
  fromSlaw s =
    case fromSlaw s of
      Right False -> return AutoFlushNever
      Right True  -> return AutoFlushAlways
      Left  _     -> enumFromSlaw afStrs s

instance ToSlaw AutoFlush where
  toSlaw = enumToSlaw afStrs

afStrs :: EnumStrings AutoFlush
afStrs = makeEnumStrings
  [ ("never  n",                        AutoFlushNever)
  , ("always a",                        AutoFlushAlways)
  , ("if-not-seekable IfNotSeekable i", AutoFlushIfNotSeekable)
  ]

--

data StrOrInt = StringValue  !T.Text
              | NumericValue !Integer
              deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

instance Default StrOrInt where
  def = NumericValue (-1)

instance Nameable StrOrInt where
  typeName _ =   "StrOrInt"

instance FromSlaw StrOrInt where
  fromSlaw (SlawString    utf8) = (return . StringValue . fromUtf8) utf8
  fromSlaw s@(SlawNumeric _ _ ) = second NumericValue $ fromSlaw s
  fromSlaw s                    = handleOthers s

instance ToSlaw StrOrInt where
  toSlaw (NumericValue n)  = preferNumeric NumInt32 n
  toSlaw (StringValue txt) = SlawString $ toUtf8 txt

--

data FileFormat = BinaryFile | YamlFile
                deriving (Eq, Ord, Show, Read, Bounded, Enum,
                          Generic, NFData, Hashable)

instance Default FileFormat where
  def = BinaryFile

instance Nameable FileFormat where
  typeName _ = "FileFormat"

instance FromSlaw FileFormat where
  fromSlaw = enumFromSlaw ffStrs

instance ToSlaw FileFormat where
  toSlaw = enumToSlaw ffStrs

ffStrs :: EnumStrings FileFormat
ffStrs = makeEnumStrings
  [ ("binary bin b", BinaryFile)
  , ("yaml  text y", YamlFile)
  ]

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

enumFromSlaw :: Nameable a
             => EnumStrings a
             -> Slaw
             -> Either PlasmaException a
enumFromSlaw es (SlawString lbs) =
  case stringToEnum es lbs of
    Just x  -> Right x
    Nothing ->
      let expected = getEnumStrings es
      in Left $ invalidArgument1 lbs (expected :: [SBS.ShortByteString])
enumFromSlaw _  s                = handleOthers s

enumToSlaw :: (Show a, Enum a) => EnumStrings a -> a -> Slaw
enumToSlaw es x =
  case enumToString es x of
    Just lbs -> SlawString lbs
    Nothing  -> (SlawString . toUtf8 . show) x -- shouldn't happen

--

data Option r = Option
  { optName :: T.Text
  , optGet  :: r -> Maybe Slaw
  , optSet  :: r -> Slaw -> r
  }

type Options r = [Option r]

opt :: (FromSlaw t, ToSlaw t)
    => T.Text
    -> (r -> Maybe t)
    -> (r -> Maybe t -> r)
    -> Option r
opt name getter setter = opt1 name getter setter š ŝm

optN :: (Integral a, Bounded a, Storable a)
     => T.Text
     -> (r -> Maybe Integer)
     -> (r -> Maybe Integer -> r)
     -> (S.Vector a -> NumericData)
     -> Option r
optN name getter setter pref = opt1 name getter setter pn ŝm
  where pn = preferNumeric pref

opt1 :: T.Text
     -> (r -> Maybe t)
     -> (r -> Maybe t -> r)
     -> (t -> Slaw)
     -> (Slaw -> Maybe t)
     -> Option r
opt1 name getter setter tSlaw fSlaw =
  Option { optName = name
         , optGet  = fmap tSlaw . getter
         , optSet  = oset
         }
  where
    oset x s = case fSlaw s of
                 Nothing -> x
                 Just v  -> x `setter` (Just v)

recordFromMap :: Default r
              => Options r
              -> Slaw
              -> r
recordFromMap opts = recordFromMap0 opts def

recordFromMap0 :: Options r
               -> r
               -> Slaw
               -> r
recordFromMap0 opts dflt = rfm1 dflt opts . coerceToMap

rfm1 :: r -> Options r -> Slaw -> r
rfm1 !x []       _ = x
rfm1 !x (o:rest) s =
  case s !? optName o of
    Nothing -> rfm1           x    rest s
    Just v  -> rfm1 (optSet o x v) rest s

recordToMap :: Options r
            -> r
            -> Slaw
recordToMap opts = SlawMap . recordToPairs opts

recordToMapWithFmt :: Options r
                   -> FileFormat
                   -> r
                   -> Slaw
recordToMapWithFmt opts fmt x = SlawMap (fmtPair : pairs)
  where
    fmtPair = (š kFormat, š fmt)
    pairs = recordToPairs opts x

recordToPairs :: Options r
             -> r
             -> [(Slaw, Slaw)]
recordToPairs opts x = mapMaybe (rtm1 x) opts

rtm1 :: r -> Option r -> Maybe (Slaw, Slaw)
rtm1 x o =
  case optGet o x of
    Nothing -> Nothing
    Just s  -> Just (toSlaw (optName o), s)

coerceToMap :: Slaw -> Slaw
coerceToMap (SlawProtein _ (Just ing) _) = coerceToMap ing
coerceToMap (SlawProtein _ Nothing    _) = SlawMap []
coerceToMap SlawNil                      = SlawMap []
coerceToMap (SlawList xs)                = SlawMap $ mapMaybe f xs
  where f (SlawCons k v) = Just (k, v)
        f _              = Nothing
coerceToMap other                        = other

kFormat :: T.Text
kFormat = "format"
