{-|
Module      : Data.Slaw
Description : Primary API for manipulating slawx
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
    --
    -- | These functions convert various Haskell types (which have
    -- 'ToSlaw' and/or 'FromSlaw' instances) to and from the 'Slaw' type.
    -- Unicode characters are used to give these very short names.
    -- @š@ (arrow points towards the @s@) means “to slaw”, while
    -- @ŝ@ (arrow points away from the @s@) means “from slaw”.
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
    -- *** Specify an explicit ErrLocation
    --
    -- | As above, but each function accepts an 'ErrLocation' to indicate
    -- where the slaw came from.  This allows the location to be
    -- accurately reported in error messages.
  , decodeSlaw'
  , decodeProtein'
  , decodeSlawLength'
  , decodeProteinLength'
    -- ** Encoding
  , encodeSlaw
    -- ** Validation
  , ValidationFlag(..)
  , validateSlaw
    -- * Typedefs
  , Utf8Str
  , BinarySlaw
  , RudeData
  , Symbol
  , minSymbol
  , maxSymbol
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
    --
    -- | These make use of the @RankNTypes@ language extension.
    --
    -- 'mapNumericData' and 'fromNumericData' are just special
    -- cases of 'mapNumericData''.
    --
    -- Similarly, 'mapNumericData'' is just a special case of
    -- 'mapNumericData2'', where both of the functions are the
    -- same.  Therefore, 'mapNumericData2'' is the most general
    -- of all of these functions.
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
    --
    -- | 2-, 3-, and 4-dimensional mathematical vectors, not to be
    -- confused with "Data.Vector", which is an entirely different
    -- animal.
  , V2(..)
  , V3(..)
  , V4(..)
    -- * Proteins
  , Protein(..)
  , protein
    -- * Exceptions
  , PlasmaException(..)
  , displayPlasmaException
  , PlasmaExceptionType(..)
  , ErrLocation(..)
  , displayErrLocation
  , DataSource(..)
  , displayDataSource
  , Retort(..)
    -- * Byte Order
  , ByteOrder(..)
  , nativeByteOrder
  , oppositeByteOrder
    -- * Typeclasses
  , Nameable(..)
  , FromSlaw(..)
  , ToSlaw(..)
    -- ** Numeric classes
  , PlasmaReal(..)
  , PlasmaIntegral
  , PlasmaFloat(..)
  , PlasmaScalar(..)
  , PlasmaNumeric(..)
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
