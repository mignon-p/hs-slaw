module Data.Slaw.Internal.SlawIO
  ( SlawInputStream(..)
  , siRead
  , SlawOutputStream(..)
  , openBinarySlawInput
  , openBinarySlawOutput
  , fileMagic
  , binaryFileTypeSlaw
  , currentSlawVersion
  ) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as R
import qualified Data.ByteString.Lazy    as L
import Data.Default.Class
import Data.Word
import GHC.Stack
import System.IO
import Text.Printf

import Data.Slaw.Internal.Bitfield
import Data.Slaw.Internal.BitfieldDefs
import Data.Slaw.Internal.Exception
import Data.Slaw.Internal.FileClass
import Data.Slaw.Internal.OptionRecords
import Data.Slaw.Internal.OptionTypes
import Data.Slaw.Internal.SlawConvert
import Data.Slaw.Internal.SlawDecode
-- import Data.Slaw.Internal.SlawEncode
-- import Data.Slaw.Internal.SlawPath
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
  , siRead' :: CallStack -> IO (Maybe Slaw)
  , siClose :: IO ()
  }

instance Show SlawInputStream where
  showsPrec n x = showParen (n > 10) s
    where s = showString "SlawInputStream " . showString (siName x)

siRead :: HasCallStack => SlawInputStream -> IO (Maybe Slaw)
siRead si = siRead' si callStack

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

data SOutput = SOutput
  { soutName   :: String
  , soutOrder  :: !ByteOrder
  , soutFlush  :: !Bool
  , soutHandle :: !Handle
  , soutClose  :: !Bool
  }

openBinarySlawInput :: (HasCallStack, FileClass a, ToSlaw b)
                    => a
                    -> b -- options map/protein (currently none)
                    -> IO SlawInputStream
openBinarySlawInput file _ = withFrozenCallStack $ do
  let nam = fcName file
  eth <- fcOpenReadOrMap file
  rdr <- makeFileReader eth
  inp <- makeSInput nam rdr
  return $ SlawInputStream { siName  = nam
                           , siRead' = readSInput  inp
                           , siClose = closeSInput inp
                           }

makeSInput :: HasCallStack => String -> FileReader -> IO SInput
makeSInput nam rdr = do
  off <- fromIntegral <$> getOffset rdr
  lbs <- readBytes rdr 8
  let bs       = L.toStrict lbs
      thr off2 = throwIO . slawIOException nam (off + off2)
      hdrLen   = B.length bs
  when (hdrLen /= 8) $ do
    let msg = "unexpected end of file: could not read header"
    thr (fromIntegral hdrLen) msg
  let o    = decodeOct BigEndian   bs
      mag  = getBf'    bfMagic     o
      vers = getBf'    bfVersion   o
      typ  = getBf'    bfType      o
      big  = getBfBool bfBigEndian o
  when (mag /= fileMagic) $ do
    let msg = printf "did not begin with magic number %08X" fileMagic
    thr 0 msg
  when (typ /= binaryFileTypeSlaw) $ do
    let msg = "not a binary slaw file"
    thr 5 msg
  when (vers /= currentSlawVersion) $ do
    let msg = printf
              "file contains slaw version %u, but only %u is supported"
              (vers :: Word8)
              currentSlawVersion
    thr 4 msg
  return $ SInput { sinName   = nam
                  , sinOrder  = if big then BigEndian else LittleEndian
                  , sinReader = rdr
                  }

slawIOException :: HasCallStack
                => String
                -> Word64
                -> String
                -> PlasmaException
slawIOException nam off = slawIOException' callStack (mkLoc nam off)

slawIOException' :: CallStack
                 -> ErrLocation
                 -> String
                 -> PlasmaException
slawIOException' cs el msg = def
  { peType      = EtSlawIO
  , peMessage   = msg
  , peCallstack = Just cs
  , peLocation  = Just el
  }

mkLoc :: String -> Word64 -> ErrLocation
mkLoc nam off = ErrLocation
  { elSource = DsFile nam
  , elOffset = Just   off
  }

readSInput :: SInput -> CallStack -> IO (Maybe Slaw)
readSInput inp cs = do
  let nam = sinName   inp
      bo  = sinOrder  inp
      rdr = sinReader inp
  off <- fromIntegral <$> getOffset rdr
  lbs <- peekBytes rdr 8
  let bs     = L.toStrict lbs
      hdrLen = B.length bs
  if | hdrLen == 0 -> return Nothing
     | hdrLen /= 8 -> do
         let msg = "unexpected end of file: could not read slaw"
         throwIO $ slawIOException nam (off + fromIntegral hdrLen) msg
     | otherwise   -> do
         let o = decodeOct bo bs
         case lengthFromHeader o of
           Left msg -> do
             let exc = corruptSlaw msg $ mkLoc nam off
             throwIO $ exc { peCallstack = Just cs }
           Right octLen -> Just <$> readSInput1 inp cs off octLen

readSInput1 :: SInput -> CallStack -> Word64 -> Word64 -> IO Slaw
readSInput1 inp cs off octLen = do
  let nam = sinName   inp
      bo  = sinOrder  inp
      rdr = sinReader inp
      len = 8 * octLen
      el  = mkLoc nam off
  lbs <- readBytes rdr (fromIntegral len)
  when (L.length lbs /= fromIntegral len) $ do
    let msg = printf
              "%s: expected %u bytes but only read %d bytes"
              ("unexpected end of file" :: String)
              len
              (L.length lbs)
    throwIO $ slawIOException' cs el msg
  return $ decodeSlaw' bo el lbs

closeSInput :: SInput -> IO ()
closeSInput = closeFileReader . sinReader

openBinarySlawOutput :: (FileClass a, ToSlaw b)
                     => a
                     -> b -- options map/protein
                     -> IO SlawOutputStream
openBinarySlawOutput file opts = do
  let opts' = toSlaw opts
      wbo   = ŝm     opts' ?> def
      nam   = fcName file
  (h, shouldClose) <- fcOpenWrite file
  out <- makeSOutput nam (h, shouldClose) wbo
  return $ SlawOutputStream { soName = nam
                            , soWrite = writeSOutput out
                            , soFlush = flushSOutput out
                            , soClose = closeSOutput out
                            }

getBo' :: PreferredByteOrder -> ByteOrder
getBo' BoNative       = nativeByteOrder
getBo' BoLittleEndian = LittleEndian
getBo' BoBigEndian    = BigEndian

getBo :: WriteBinaryOptions -> ByteOrder
getBo = getBo' . wboByteOrder

makeSOutput :: String
            -> (Handle, Bool)
            -> WriteBinaryOptions
            -> IO SOutput
makeSOutput nam (h, shouldClose) wbo = do
  let bo = getBo wbo
      o  = sum [ makeBf'    bfMagic     fileMagic
               , makeBf'    bfVersion   currentSlawVersion
               , makeBf'    bfType      binaryFileTypeSlaw
               , makeBfBool bfBigEndian (bo == BigEndian)
               ]
      bld = R.word64BE o
  autoFlush <- case wboAutoFlush wbo of
                 AutoFlushNever  -> return False
                 AutoFlushAlways -> return True
                 AutoFlushIfNotSeekable -> do
                   eth <- tryIO $ hTell h
                   case eth of
                     Left  _ -> return True
                     Right _ -> return False
  R.hPutBuilder h bld
  return $ SOutput { soutName   = nam
                   , soutOrder  = getBo wbo
                   , soutFlush  = autoFlush
                   , soutHandle = h
                   , soutClose  = shouldClose
                   }

writeSOutput :: SOutput -> Slaw -> IO ()
writeSOutput = undefined

flushSOutput :: SOutput -> IO ()
flushSOutput = undefined

closeSOutput :: SOutput -> IO ()
closeSOutput = undefined
