{-|
Module      : Data.Slaw.Internal
Description :
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module Data.Slaw.Internal
  ( -- * Slaw decoding (can specify an ErrLocation)
    decodeSlaw'
  , decodeProtein'
  , decodeSlawLength'
  , decodeProteinLength'
    -- * Strings
  , Utf8Str
  , TextClass(..)
  , ByteStringClass(..)
  , indentLines
  , stdIndent
    -- * Util
  , (##)
  , (??)
  , (?>)
  , tryIO
    -- * IO
  , FileClass(..)
  , FileReader(..)
  , HPair
  , makeFileReader
  , readBytes
  , peekBytes
  , closeFileReader
  , getOffset
    -- * Options
  , StrNumNone(..)
  , FileFormat(..)
  , WriteYamlOptions(..)
  , getFileFormat
  , PoolCreateOptions(..)
  ) where

import Data.Slaw.Internal.Exception
import Data.Slaw.Internal.FileClass
-- import Data.Slaw.Internal.Helpers
-- import Data.Slaw.Internal.Nameable
-- import Data.Slaw.Internal.NumericConvert
import Data.Slaw.Internal.OptionRecords
import Data.Slaw.Internal.OptionTypes
-- import Data.Slaw.Internal.SlawConvert
import Data.Slaw.Internal.SlawDecode
-- import Data.Slaw.Internal.SlawEncode
-- import Data.Slaw.Internal.SlawIO
-- import Data.Slaw.Internal.SlawType
import Data.Slaw.Internal.String
import Data.Slaw.Internal.Util
-- import Data.Slaw.Internal.Validation
-- import Data.Slaw.Internal.VectorTypes
