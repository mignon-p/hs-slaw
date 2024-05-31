module Data.Slaw.Internal.Exception
  ( DataSource(..)
  , displayDataSource
  , ErrLocation(..)
  , displayErrLocation
  , PlasmaException(..)
  , displayPlasmaException
  , corruptSlaw
  , typeMismatch
  , typeMismatch'
  , typeMismatchPfx
  , rangeErrorPfx
  , because
  , because1
  , cantCoerce
  , rangeError0
  , rangeError
  , rangeError'
  ) where

import Control.Exception
import Control.DeepSeq
import Data.Default.Class
import Data.Hashable
import Data.Int
import Data.List
import Data.Word
import GHC.Generics (Generic)
import GHC.Stack

import Data.Slaw.Internal.String
import Data.Slaw.Internal.Util

data DataSource = DsFile       { dsName  :: String }
                | DsPool       { dsName  :: String
                               , dsIndex :: Maybe Int64
                               }
                | DsOther      { dsName  :: String }
                | DsNone
                deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance Default DataSource where
  def = DsNone

displayDataSource :: DataSource -> String
displayDataSource DsNone                   = "unknown"
displayDataSource (DsPool name  Nothing  ) = name
displayDataSource (DsPool name (Just idx)) = name ++ "#" ++ show idx
displayDataSource x                        = dsName x

data ErrLocation = ErrLocation
  { elSource :: DataSource
  , elOffset :: Maybe Word64
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance Default ErrLocation where
  def = ErrLocation DsNone Nothing

displayErrLocation :: ErrLocation -> String
displayErrLocation (ErrLocation ds  Nothing  ) = displayDataSource ds
displayErrLocation (ErrLocation ds (Just off)) =
  displayDataSource ds ++ "@" ++ show off

displayMaybeErrLocation :: String -> Maybe ErrLocation -> String
displayMaybeErrLocation _    Nothing                            = ""
displayMaybeErrLocation _   (Just (ErrLocation DsNone Nothing)) = ""
displayMaybeErrLocation sep (Just loc)                          =
  displayErrLocation loc ++ sep

data PlasmaException = PlasmaException
  { peType      :: !PlasmaExceptionType
  , peRetort    :: Maybe Int64
  , peMessage   :: String
  , peCallstack :: Maybe CallStack   -- location of code
  , peLocation  :: Maybe ErrLocation -- location of data
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

displayPlasmaException :: Bool -> PlasmaException -> String
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

data PlasmaExceptionType = EtCorruptSlaw
                         | EtTypeMismatch
                         | EtSlawIO
                         | EtPools
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

typeMismatch' :: String -> ErrLocation -> PlasmaException
typeMismatch' msg loc = def { peType      = EtTypeMismatch
                            , peMessage   = msg
                            , peLocation  = Just loc
                            }

typeMismatchPfx :: String
typeMismatchPfx = "type mismatch: "

rangeErrorPfx :: String
rangeErrorPfx = "range error: "

because :: String -> [PlasmaException] -> Either PlasmaException a
because msg = Left . because1 msg

because1 :: String -> [PlasmaException] -> PlasmaException
because1 msg reasons = typeMismatch msg'
  where msg'     = concat [ msg, ", because:\n", reasons']
        reasons0 = map (indentLines "  " . peMessage) reasons
        reasons' = intercalate "\nand:\n" reasons0

cantCoerce :: String -> String -> String
cantCoerce desc other = concat ["Can't coerce ", desc, " to ", other]

rangeError0 :: String -> PlasmaException
rangeError0 msg = def { peType      = EtTypeMismatch
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
