{-|
Module      : Data.Slaw
Description :
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module Data.Slaw
  ( -- * Slaw
    Slaw(..)
  , describeSlaw
    -- ** Predicates
  , isProtein
  , isBool
  , isNil
  , isSymbol
  , isString
  , isList
  , isMap
  , isCons
  , isNumeric
  , isError
    -- ** Conversion
  , š
  , ŝ
  , ŝm
  , ŝes
  , ŝee
  , (?:)
    -- ** Decoding
  , decodeSlaw
  , decodeProtein
  , decodeSlawLength
  , decodeProteinLength
    -- ** Encoding
  , encodeSlaw
    -- ** Validation
  , ValidationFlag(..)
  , validateSlaw
    -- * Typedefs
  , Symbol
  , RudeData
  , BinarySlaw
  , Utf8Str
    -- * Other types
  , Protein(..)
  , protein
    -- * Numeric types
  , NumericFormat(..)
  , NumericData(..)
  , VectorType(..)
  , isNumericFormatLegal
  , vectorSize
  , numericFormatSize
  , describeNumeric
  , describeNumericFormat
  , describeNumericData
    -- ** Operations on NumericData
  , mapNumericData
  , mapNumericData2
  , mapNumericData'
  , mapNumericData2'
  , fromNumericData
  , fromNumericData2
  , toNumericData
  , sliceNumericData
  , lengthNumericData
    -- ** Vector types
  , V2(..)
  , V3(..)
  , V4(..)
    -- * Exceptions
  , PlasmaException(..)
  , displayPlasmaException
  , ErrLocation(..)
  , displayErrLocation
  , DataSource(..)
  , displayDataSource
    -- * Byte Order
  , ByteOrder(..)
  , nativeByteOrder
  , oppositeByteOrder
    -- * Typeclasses
  , Nameable(..)
  , PlasmaReal(..)
  , PlasmaIntegral
  , PlasmaFloat(..)
  , PlasmaScalar(..)
  , PlasmaNumeric(..)
  , FromSlaw(..)
  , ToSlaw(..)
  ) where

import Data.Slaw.Internal.Exception
import Data.Slaw.Internal.HalfOrphans ()
import Data.Slaw.Internal.Helpers
import Data.Slaw.Internal.Nameable
import Data.Slaw.Internal.NumericConvert
import Data.Slaw.Internal.SlawConvert
import Data.Slaw.Internal.SlawDecode
import Data.Slaw.Internal.SlawEncode
import Data.Slaw.Internal.SlawType
import Data.Slaw.Internal.String
import Data.Slaw.Internal.Util
import Data.Slaw.Internal.Validation
import Data.Slaw.Internal.VectorTypes
