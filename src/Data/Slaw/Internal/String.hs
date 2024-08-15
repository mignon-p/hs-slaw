{-|
Module      : Data.Slaw.Internal.String
Description : Typeclasses for converting Text and ByteStrings
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Data.Slaw.Internal.String
  ( Utf8Str
  , TextClass(..)
  , ByteStringClass(..)
  , indentLines
  , withLazyByteStringAsCString
  , withLazyByteStringAsCStringNL
  , withLazyByteStringAsCStringLen
  ) where

import Control.DeepSeq
-- import Control.Exception
import Control.Monad
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as L
import qualified Data.ByteString.Short    as SBS
import qualified Data.ByteString.Unsafe   as B
import Data.Char
-- import Data.Default.Class
import Data.Hashable
import Data.String
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.Encoding  as LT
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
-- import GHC.Generics (Generic)

import Data.Slaw.Internal.Util

-- | A string, represented as a UTF-8 encoded lazy 'L.ByteString'
type Utf8Str = L.ByteString

class ( IsString a
      , Monoid   a
      , Ord      a
      , Hashable a
      , NFData   a
      ) => TextClass a where
  toString   :: a -> String
  toText     :: a -> T.Text
  toLazyText :: a -> LT.Text
  toUtf8     :: a -> Utf8Str

  -- fromString is inherited from the IsString class
  fromText     :: T.Text  -> a
  fromLazyText :: LT.Text -> a
  fromUtf8     :: Utf8Str -> a

instance TextClass String where
  toString     = id
  toText       = T.pack
  toLazyText   = LT.pack
  toUtf8       = LT.encodeUtf8 . LT.pack

  fromText     = T.unpack
  fromLazyText = LT.unpack
  fromUtf8     = LT.unpack . LT.decodeUtf8With T.lenientDecode

instance TextClass T.Text where
  toString     = T.unpack
  toText       = id
  toLazyText   = LT.fromStrict
  toUtf8       = L.fromStrict . T.encodeUtf8

  fromText     = id
  fromLazyText = LT.toStrict
  fromUtf8     = LT.toStrict . LT.decodeUtf8With T.lenientDecode

instance TextClass LT.Text where
  toString     = LT.unpack
  toText       = LT.toStrict
  toLazyText   = id
  toUtf8       = LT.encodeUtf8

  fromText     = LT.fromStrict
  fromLazyText = id
  fromUtf8     = LT.decodeUtf8With T.lenientDecode

class ( Monoid   a
      , Ord      a
      , Hashable a
      , NFData   a
      ) => ByteStringClass a where
  toByteString      :: a -> B.ByteString
  toLazyByteString  :: a -> L.ByteString
  toShortByteString :: a -> SBS.ShortByteString
  toWord8s          :: a -> [Word8]

  fromByteString      :: B.ByteString        -> a
  fromLazyByteString  :: L.ByteString        -> a
  fromShortByteString :: SBS.ShortByteString -> a
  fromWord8s          :: [Word8]             -> a

instance ByteStringClass B.ByteString where
  toByteString      = id
  toLazyByteString  = L.fromStrict
  toShortByteString = SBS.toShort
  toWord8s          = B.unpack

  fromByteString      = id
  fromLazyByteString  = L.toStrict
  fromShortByteString = SBS.fromShort
  fromWord8s          = B.pack

instance ByteStringClass L.ByteString where
  toByteString      = L.toStrict
  toLazyByteString  = id
  toShortByteString = SBS.toShort . L.toStrict
  toWord8s          = L.unpack

  fromByteString      = L.fromStrict
  fromLazyByteString  = id
  fromShortByteString = L.fromStrict . SBS.fromShort
  fromWord8s          = L.pack

instance ByteStringClass SBS.ShortByteString where
  toByteString      = SBS.fromShort
  toLazyByteString  = L.fromStrict . SBS.fromShort
  toShortByteString = id
  toWord8s          = SBS.unpack

  fromByteString      = SBS.toShort
  fromLazyByteString  = SBS.toShort . L.toStrict
  fromShortByteString = id
  fromWord8s          = SBS.pack

--

myStripSuffix :: LT.Text -> LT.Text -> LT.Text
myStripSuffix sfx txt = (sfx `LT.stripSuffix` txt) ?> txt

indentLines :: TextClass a => a -> a -> a
indentLines indent str =
  let txt             = toLazyText str
      indent'         = toLazyText indent
      trailingNewline = "\n" `LT.isSuffixOf` txt
      lns             = LT.lines txt
      lns'            = map (indent' <>) lns
      txt'            = LT.unlines lns'
  in if trailingNewline
     then fromLazyText txt'
     else fromLazyText ("\n" `myStripSuffix` txt')

--

withLazyByteStringAsCString :: L.ByteString
                            -> (CString -> IO a)
                            -> IO a
withLazyByteStringAsCString lbs func =
  withLazyByteStringAsCStringLen' False lbs (func . fst)

withLazyByteStringAsCStringNL :: L.ByteString
                              -> (CString -> IO a)
                              -> IO a
withLazyByteStringAsCStringNL lbs func =
  withLazyByteStringAsCStringLen' True lbs (func . fst)

withLazyByteStringAsCStringLen :: L.ByteString
                               -> (CStringLen -> IO a)
                               -> IO a
withLazyByteStringAsCStringLen = withLazyByteStringAsCStringLen' False

withLazyByteStringAsCStringLen' :: Bool -- add a terminating newline?
                                -> L.ByteString
                                -> (CStringLen -> IO a)
                                -> IO a
withLazyByteStringAsCStringLen' termNL lbs func = do
  let len    = L.length lbs
      extra  = 2 -- one for terminating NUL and one for optional newline
      bufLen = len + extra
  when (bufLen >= fromIntegral (maxBound :: Int)) $
    fail $ "length " ++ show bufLen ++ " is too big"
  allocaBytes (fromIntegral bufLen) $ \buf -> do
    withLbs1 termNL (L.toChunks lbs) func buf 0

withLbs1 :: Bool
         -> [B.ByteString]
         -> (CStringLen -> IO a)
         -> CString
         -> Int
         -> IO a
withLbs1 termNL [] func buf !pos = do
  haveNL <- endsWithNewline buf pos
  let needNL = termNL && not haveNL
      byte1  = if needNL then newline else 0
      byte2  = 0 :: CChar
      len    = if needNL then pos + 1 else pos
  pokeByteOff buf  pos      byte1
  pokeByteOff buf (pos + 1) byte2
  func (buf, len)
withLbs1 termNL (bs:rest) func buf !pos = do
  B.unsafeUseAsCStringLen bs $ \(src, srcLen) -> do
    copyBytes (buf `plusPtr` pos) src srcLen
    withLbs1 termNL rest func buf (pos + srcLen)

newline :: CChar
newline = (fromIntegral . ord) '\n'

endsWithNewline :: CString -> Int -> IO Bool
endsWithNewline _ 0 = return False
endsWithNewline buf pos = do
  lastByte <- peekByteOff buf (pos - 1)
  return (lastByte == newline)
