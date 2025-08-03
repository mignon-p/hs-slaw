{-|
Module      : Data.Slaw.Internal.Exception
Description : PlasmaException and related types/functions
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Slaw.Internal.Exception
  ( DataSource(..)
  , displayDataSource
  , ErrLocation(..)
  , displayErrLocation
  , PlasmaException(..)
  , displayPlasmaException
  , PlasmaExceptionType(..)
  , Retort(..)
  , corruptSlaw
  , typeMismatch
  -- , typeMismatch'
  , typeMismatchPfx
  , rangeErrorPfx
  , invalidArgumentPfx
  , etFromMsg
  , because
  , because1
  , cantCoerce
  , rangeError0
  , rangeError
  , rangeError'
  , invalidArgument
  , invalidArgument1
  , validationError
  , unicodeError
  , unicodeError1
  , notFoundErr
  , stdIndent
  , tryPE
  ) where

import Control.DeepSeq
import Control.Exception
import Data.Bits
import Data.Default.Class
import Data.Hashable
import Data.Int
import Data.List
import qualified Data.Text.Encoding.Error as T
import Data.Word
import Foreign.Storable
import GHC.Generics (Generic)
import GHC.Stack
import Text.Printf

import Data.Slaw.Internal.String
import Data.Slaw.Internal.Util

-- | Represents a 64-bit error code returned from libPlasma.
newtype Retort = Retort { unRetort :: Int64 }
               deriving newtype (Eq, Ord, Show, NFData, Hashable, Real,
                                 PrintfArg, Bits, FiniteBits, Bounded,
                                 Enum, Storable, Num, Read, Integral)

-- | Indicates the file, pool, or other resource in which an error
-- was found.
data DataSource =
    DsFile  { dsName  :: String      -- ^ name of file, pool, etc.
            }
  | DsPool  { dsName  :: String      -- ^ name of file, pool, etc.
            , dsIndex :: Maybe Int64 -- ^ protein index within pool
            }
  | DsOther { dsName  :: String      -- ^ name of file, pool, etc.
            }
  | DsNone
  deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance Default DataSource where
  def = DsNone

-- | Convert a 'DataSource' to a human-readable 'String'.
displayDataSource :: DataSource -> String
displayDataSource DsNone                   = "unknown"
displayDataSource (DsPool name  Nothing  ) = name
displayDataSource (DsPool name (Just idx)) = name ++ "#" ++ show idx
displayDataSource x                        = dsName x

-- | Indicates the file, pool, or other resource in which an error
-- was found, and optionally a byte offset into that resource.
data ErrLocation = ErrLocation
  { elSource :: DataSource   -- ^ where erroneous data came from
  , elOffset :: Maybe Word64 -- ^ optional byte offset within the above
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance Default ErrLocation where
  def = ErrLocation DsNone Nothing

-- | Convert an t'ErrLocation' to a human-readable 'String'.
displayErrLocation :: ErrLocation -> String
displayErrLocation (ErrLocation ds  Nothing  ) = displayDataSource ds
displayErrLocation (ErrLocation ds (Just off)) =
  displayDataSource ds ++ "@" ++ show off

displayMaybeErrLocation :: String -> Maybe ErrLocation -> String
displayMaybeErrLocation _    Nothing                            = ""
displayMaybeErrLocation _   (Just (ErrLocation DsNone Nothing)) = ""
displayMaybeErrLocation sep (Just loc)                          =
  displayErrLocation loc ++ sep

-- | An exception which may be raised by Plasma functions.
--
-- The 'Eq', 'Ord', and 'Hashable' instances only take
-- 'peType', 'peRetort', and 'peMessage' into account.
data PlasmaException = PlasmaException
  { -- | General category that the exception falls into.
    peType      :: !PlasmaExceptionType
    -- | Error code from @libPlasma/c@.
  , peRetort    :: Maybe Retort
    -- | Error message.
  , peMessage   :: String
    -- | Backtrace of the code that threw the exception.
  , peCallstack :: Maybe CallStack
    -- | Location within a file, pool, slaw, etc. where the
    -- error was encountered.
  , peLocation  :: Maybe ErrLocation
  } deriving (Show)

instance Ord PlasmaException where
  x `compare` y =
    (peType    x ?? peType    y) <>
    (peRetort  x ?? peRetort  y) <>
    (peMessage x ?? peMessage y)

instance Eq PlasmaException where
  x == y = (x ?? y) == EQ

instance Hashable PlasmaException where
  salt `hashWithSalt` x = salt ## peType x ## peRetort x ## peMessage x

instance NFData PlasmaException where
  rnf x =
    peType            x  `deepseq`
    peRetort          x  `deepseq`
    peMessage         x  `deepseq`
    show (peCallstack x) `deepseq`
    rnf  (peLocation  x)

instance Exception PlasmaException where
  displayException = displayPlasmaException True

-- | Convert a t'PlasmaException' to a human-readable 'String'.
displayPlasmaException :: Bool -- ^ Do you want to display the call stack?
                       -> PlasmaException -- ^ Exception to display
                       -> String
displayPlasmaException wantCallStack e =
  let msg = displayMaybeErrLocation ": " (peLocation e) ++ peMessage e
  in case (wantCallStack, peCallstack e) of
       (True, Just cs) -> msg ++ "\n" ++ prettyCallStack cs
       _               -> msg

instance Default PlasmaException where
  def = PlasmaException
        { peType      = EtOther
        , peRetort    = Nothing
        , peMessage   = ""
        , peCallstack = Nothing
        , peLocation  = Nothing
        }

-- | The type of t'PlasmaException'.
data PlasmaExceptionType =
    -- | An error occurred when decoding a slaw from binary data.
    EtCorruptSlaw
    -- | Was expecting a slaw of a certain type, but got another
    -- type instead.
  | EtTypeMismatch
    -- | Something was out of range, such as an integer which
    -- cannot be represented in a given type.
  | EtRangeError
    -- | An argument had an unacceptable value.
  | EtInvalidArgument
    -- | The given slaw did not meet criteria in 'Data.Slaw.validateSlaw'.
  | EtValidationError
    -- | Unicode encoding error (probably invalid UTF-8).
  | EtUnicodeError
    -- | Something that was requested could not be found.
  | EtNotFound
    -- | An exception occurred when reading or writing slawx
    -- to or from a file.
  | EtSlawIO
    -- | An exception occurred when accessing a pool.
  | EtPools
    -- | Something else happened.
  | EtOther
  deriving (Eq, Ord, Show, Read, Bounded, Enum,
            Generic, NFData, Hashable)

corruptSlaw :: String -> ErrLocation -> PlasmaException
corruptSlaw msg loc = def { peType      = EtCorruptSlaw
                          , peMessage   = msg
                          , peLocation  = Just loc
                          }

typeMismatch :: String -> PlasmaException
typeMismatch msg = def { peType      = EtTypeMismatch
                       , peMessage   = typeMismatchPfx ++ msg
                       }

{-
typeMismatch' :: String -> ErrLocation -> PlasmaException
typeMismatch' msg loc = def { peType      = EtTypeMismatch
                            , peMessage   = msg
                            , peLocation  = Just loc
                            }
-}

typeMismatchPfx :: String
typeMismatchPfx = "type mismatch: "

rangeErrorPfx :: String
rangeErrorPfx = "range error: "

invalidArgumentPfx :: String
invalidArgumentPfx = "invalid argument: "

validationErrorPfx :: String
validationErrorPfx = "validation error: "

unicodeErrorPfx :: String
unicodeErrorPfx = "invalid UTF-8: "

-- This is a hack.  Guess the exception type based on the message.
etFromMsg :: String -> PlasmaExceptionType
etFromMsg msg
  | typeMismatchPfx    `isPrefixOf` msg = EtTypeMismatch
  | rangeErrorPfx      `isPrefixOf` msg = EtRangeError
  | invalidArgumentPfx `isPrefixOf` msg = EtInvalidArgument
  | validationErrorPfx `isPrefixOf` msg = EtValidationError
  | unicodeErrorPfx    `isPrefixOf` msg = EtUnicodeError
  | otherwise                           = EtCorruptSlaw

because :: String -> [PlasmaException] -> Either PlasmaException a
because msg = Left . because1 msg

because1 :: String -> [PlasmaException] -> PlasmaException
because1 msg reasons = typeMismatch msg'
  where msg'     = concat [ msg, ", because:\n", reasons']
        reasons0 = map (indentLines stdIndent . peMessage) reasons
        reasons' = intercalate "\nand:\n" reasons0

cantCoerce :: String -> String -> String
cantCoerce desc other = concat ["Can't coerce ", desc, " to ", other]

rangeError0 :: String -> PlasmaException
rangeError0 msg = def { peType      = EtRangeError
                      , peMessage   = rangeErrorPfx ++ msg
                      }

rangeError :: String
           -> String
           -> String
           -> String
           -> String
           -> PlasmaException
rangeError fromType fromValue toType lo hi = rangeError0 msg
  where
    msg = concat [ fromType
                 , " "
                 , fromValue
                 , " is not in the range of "
                 , toType
                 , " ["
                 , lo
                 , ".."
                 , hi
                 , "]"
                 ]

rangeError' :: (String, String, String, String, String)
            -> PlasmaException
rangeError' = uncurry5 rangeError

invalidArgument :: String -> PlasmaException
invalidArgument msg = def { peType      = EtInvalidArgument
                          , peMessage   = invalidArgumentPfx ++ msg
                          }

invalidArgument1 :: (Show a, Show b)
                 => a
                 -> [b]
                 -> PlasmaException
invalidArgument1 got expected = invalidArgument msg
  where msg = concat $ [ "got "
                       , show got
                       , " but expected one of "
                       ] ++ intersperse ", " (map show expected)

validationError :: String -> PlasmaException
validationError msg = def { peType      = EtValidationError
                          , peMessage   = validationErrorPfx ++ msg
                          }

unicodeError :: String -> PlasmaException
unicodeError msg = def { peType      = EtUnicodeError
                       , peMessage   = unicodeErrorPfx ++ msg
                       }

unicodeError1 :: T.UnicodeException -> PlasmaException
unicodeError1 = unicodeError . displayException

notFoundErr :: String -> PlasmaException
notFoundErr msg = def { peType      = EtNotFound
                      , peMessage   = msg
                      }

-- | Amount to use when indenting error messages.
-- Currently, two spaces.
stdIndent :: String
stdIndent = "  "

{-# INLINE tryPE #-}
-- | This handy utility function is just 'try', but constrained
-- to only work on t'PlasmaException'.
tryPE :: IO a -> IO (Either PlasmaException a)
tryPE = try
