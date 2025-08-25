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
  ) where

import Control.DeepSeq
-- import Control.Exception
-- import Control.Monad
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as L
import qualified Data.ByteString.Short    as SBS
-- import qualified Data.ByteString.Unsafe   as B
-- import Data.Char
-- import Data.Default.Class
import Data.Hashable
import Data.String
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.Builder   as R
import qualified Data.Text.Lazy.Encoding  as LT
import Data.Word
-- import Foreign.C.String
-- import Foreign.C.Types
-- import Foreign.Marshal.Alloc
-- import Foreign.Marshal.Utils
-- import Foreign.Ptr
-- import Foreign.Storable
-- import GHC.Generics (Generic)

import Data.Slaw.Internal.Util

-- | A string, represented as a UTF-8 encoded lazy 'L.ByteString'.
type Utf8Str = L.ByteString

-- | Represents a string of Unicode characters (code points), and
-- provides methods to convert it to and from other representations.
class ( IsString a
      , Monoid   a
      , Ord      a
      ) => TextClass a where
  toString   :: a -> String
  toText     :: a -> T.Text
  toLazyText :: a -> LT.Text
  toUtf8     :: a -> Utf8Str
  toTxtBld   :: a -> R.Builder

  -- fromString is inherited from the IsString class
  fromText     :: T.Text    -> a
  fromLazyText :: LT.Text   -> a
  fromUtf8     :: Utf8Str   -> a
  fromTxtBld   :: R.Builder -> a
  fromTxtBld   = fromLazyText . R.toLazyText

instance TextClass String where
  toString     = id
  toText       = T.pack
  toLazyText   = LT.pack
  toUtf8       = LT.encodeUtf8 . LT.pack
  toTxtBld     = R.fromString

  fromText     = T.unpack
  fromLazyText = LT.unpack
  fromUtf8     = LT.unpack . LT.decodeUtf8With T.lenientDecode

instance TextClass T.Text where
  toString     = T.unpack
  toText       = id
  toLazyText   = LT.fromStrict
  toUtf8       = L.fromStrict . T.encodeUtf8
  toTxtBld     = R.fromText

  fromText     = id
  fromLazyText = LT.toStrict
  fromUtf8     = LT.toStrict . LT.decodeUtf8With T.lenientDecode

instance TextClass LT.Text where
  toString     = LT.unpack
  toText       = LT.toStrict
  toLazyText   = id
  toUtf8       = LT.encodeUtf8
  toTxtBld     = R.fromLazyText

  fromText     = LT.fromStrict
  fromLazyText = id
  fromUtf8     = LT.decodeUtf8With T.lenientDecode

instance TextClass R.Builder where
  toString     = LT.unpack     . R.toLazyText
  toText       = LT.toStrict   . R.toLazyText
  toLazyText   =                 R.toLazyText
  toUtf8       = LT.encodeUtf8 . R.toLazyText
  toTxtBld     = id

  fromText     = R.fromText
  fromLazyText = R.fromLazyText
  fromUtf8     = R.fromLazyText . LT.decodeUtf8With T.lenientDecode
  fromTxtBld   = id

-- | Represents a string of bytes ('Word8'), and provides
-- methods to convert it to and from other representations.
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

-- | Indents all lines of a string (which may contain embedded
-- newlines) by prepending a specified string (which is typically
-- just some number of spaces) to the beginning of each line.
indentLines :: TextClass a
            => a -- ^ Indentation to add to each line.
            -> a -- ^ (Potentially multi-line) string to indent.
            -> a
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
