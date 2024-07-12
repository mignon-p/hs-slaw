module Data.Slaw.Internal.SlawIO
  ( SlawInputStream(..)
  , SlawOutputStream(..)
  , openBinarySlawInput
  , openBinarySlawOutput
  , fileMagic
  , binaryFileTypeSlaw
  , currentSlawVersion
  ) where

import Data.Word
-- import System.IO

import Data.Slaw.Internal.FileClass
import Data.Slaw.Internal.SlawConvert
import Data.Slaw.Internal.SlawType
import Data.Slaw.Internal.Util

fileMagic :: Word32
fileMagic = 0xffff0b10

binaryFileTypeSlaw :: Word8
binaryFileTypeSlaw = 1

currentSlawVersion :: Word8
currentSlawVersion = 2

data SlawInputStream = SlawInputStream
  { siName  :: String
  , siRead  :: IO Slaw
  , siClose :: IO ()
  }

instance Show SlawInputStream where
  showsPrec n x = showParen (n > 10) s
    where s = showString "SlawInputStream " . showString (siName x)

data SlawOutputStream = SlawOutputStream
  { soName  :: String
  , soWrite :: Slaw -> IO ()
  , soFlush :: IO ()
  , soClose :: IO ()
  }

instance Show SlawOutputStream where
  showsPrec n x = showParen (n > 10) s
    where s = showString "SlawOutputStream " . showString (soName x)

data SInput = SInput
  { sinName   :: String
  , sinOrder  :: !ByteOrder
  , sinReader :: FileReader
  }

{-
data SOutput = SOutput
  { soutName   :: String
  , soutOrder  :: !ByteOrder
  , soutHandle :: !Handle
  , soutClose  :: !Bool
  }
-}

openBinarySlawInput :: (FileClass a, ToSlaw b)
                    => a
                    -> b -- options map/protein (currently none)
                    -> IO SlawInputStream
openBinarySlawInput file _ = do
  let nam = fcName file
  eth <- fcOpenReadOrMap file
  rdr <- makeFileReader eth
  inp <- makeSInput nam rdr
  return $ SlawInputStream { siName  = nam
                           , siRead  = readSInput  inp
                           , siClose = closeSInput inp
                           }

makeSInput :: String -> FileReader -> IO SInput
makeSInput = undefined

readSInput :: SInput -> IO Slaw
readSInput = undefined

closeSInput :: SInput -> IO ()
closeSInput = closeFileReader . sinReader

openBinarySlawOutput :: (FileClass a, ToSlaw b)
                     => a
                     -> b
                     -> IO SlawOutputStream
openBinarySlawOutput = undefined

