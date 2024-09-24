{-|
Module      : Data.Slaw.Internal
Description : For use by Plasma package only
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

{-# OPTIONS_HADDOCK not-home            #-}

module Data.Slaw.Internal
  ( -- * Strings
    indentLines
  , stdIndent
    -- ** EnumStrings
  , EnumStrings
  , makeEnumStrings
  , stringToEnum
    -- * Slaw
  , handleOthers
  , preferNumeric
    -- * Util
  , tryIO
  , orList
    -- * IO
  , SlawInputStream(..)
  , SlawOutputStream(..)
  , FileClass(..)
  , HPair
  , readAllSlawx
  , fileMagic
  , openBinarySlawInput1
    -- ** FileReader
  , FileReader(..)
  , makeFileReader
  , makeFileReaderLazyBS
  , readBytes
  , peekBytes
  , closeFileReader
  , getOffset
    -- * Options
  , AutoFlush(..)
  , FileFormat(..)
  , Option
  , Options
  , kFormat
  , recordFromMap
  , recordToMap
  , recordToMapWithFmt
  , opt
  , optN
  , coerceToMap
  ) where

import Data.Slaw.Internal.EnumStrings
import Data.Slaw.Internal.Exception
import Data.Slaw.Internal.FileClass
-- import Data.Slaw.Internal.Helpers
-- import Data.Slaw.Internal.Nameable
-- import Data.Slaw.Internal.NumericConvert
-- import Data.Slaw.Internal.OptionRecords
import Data.Slaw.Internal.OptionTypes
import Data.Slaw.Internal.SlawConvert
-- import Data.Slaw.Internal.SlawDecode
-- import Data.Slaw.Internal.SlawEncode
import Data.Slaw.Internal.SlawIO
-- import Data.Slaw.Internal.SlawType
import Data.Slaw.Internal.String
import Data.Slaw.Internal.Util
-- import Data.Slaw.Internal.Validation
-- import Data.Slaw.Internal.VectorTypes
