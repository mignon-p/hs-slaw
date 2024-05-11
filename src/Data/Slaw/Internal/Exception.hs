module Data.Slaw.Internal.Exception
  ( PlasmaException(..)
  , corruptSlaw
  , cantCoerce
  ) where

import Control.Exception
import Control.DeepSeq
import Data.Default.Class
import Data.Hashable
import Data.Int
import Data.Word
import GHC.Generics (Generic)
import GHC.Stack

import Data.Slaw.Internal.Util

data PlasmaException = PlasmaException
  { peType      :: !PlasmaExceptionType
  , peRetort    :: Maybe Int64
  , peMessage   :: String
  , peCallstack :: Maybe CallStack
  , peFilename  :: Maybe String
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
    rnf  (peFilename  x)

instance Exception PlasmaException where
  displayException e =
    let msg = case peFilename e of
                Nothing -> peMessage e
                Just fn -> fn ++ ": " ++ peMessage e
    in case peCallstack e of
         Nothing -> msg
         Just cs -> msg ++ "\n" ++ prettyCallStack cs

instance Default PlasmaException where
  def = PlasmaException
        { peType      = EtOther
        , peRetort    = Nothing
        , peMessage   = ""
        , peCallstack = Nothing
        , peFilename  = Nothing
        }

data PlasmaExceptionType = EtCorruptSlaw
                         | EtCantCoerce
                         | EtSlawIO
                         | EtPools
                         | EtOther
                         deriving (Eq, Ord, Show, Read, Bounded, Enum,
                                   Generic, NFData, Hashable)

corruptSlaw :: HasCallStack => String -> PlasmaException
corruptSlaw msg = def { peType      = EtCorruptSlaw
                      , peMessage   = msg
                      , peCallstack = Just callStack
                      }

cantCoerce :: HasCallStack => String -> PlasmaException
cantCoerce msg = def { peType      = EtCantCoerce
                     , peMessage   = msg
                     , peCallstack = Just callStack
                     }
