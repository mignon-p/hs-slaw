module Data.Slaw.Internal.EnumStrings
  ( EnumStrings
  , makeEnumStrings
  , stringToEnum
  , enumToString
  , getEnumStrings
  ) where

import Control.DeepSeq
import Data.Bifunctor
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as B8
import qualified Data.ByteString.Short    as SBS
import qualified Data.HashMap.Strict      as HM
import qualified Data.IntMap.Strict       as IM
import Data.Word
import GHC.Generics (Generic)

import Data.Slaw.Internal.String

data EnumStrings a = EnumStrings
  { esFromString :: HM.HashMap SBS.ShortByteString a
  , esToString   :: IM.IntMap  B.ByteString
  } deriving (Eq, Show, Generic, NFData)

makeEnumStrings :: Enum a => [(B.ByteString, a)] -> EnumStrings a
makeEnumStrings pairs = EnumStrings efs ets
  where pairs'      = map (first B8.words) pairs
        efs         = HM.fromList $ concatMap f pairs'
        f (strs, x) = map (g x) strs
        g x bs      = (SBS.pack $ map lcAscii8 $ B.unpack bs, x)
        ets         = IM.fromList $ map h pairs'
        h (strs, x) = (fromEnum x, head strs)

stringToEnum :: ByteStringClass b => EnumStrings a -> b -> Maybe a
stringToEnum es bs = HM.lookup k (esFromString es)
  where k = SBS.pack $ map lcAscii8 $ toWord8s bs

enumToString :: (Enum a, ByteStringClass b)
             => EnumStrings a
             -> a
             -> Maybe b
enumToString es x =
  fmap fromByteString $ IM.lookup (fromEnum x) (esToString es)

getEnumStrings :: ByteStringClass b => EnumStrings a -> [b]
getEnumStrings = map fromByteString . IM.elems . esToString

lcAscii8 :: Word8 -> Word8
lcAscii8 c
  | c >= 0x41 && c <= 0x5A = c + 0x20
  | otherwise              = c
