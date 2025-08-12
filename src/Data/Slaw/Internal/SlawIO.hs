{-|
Module      : Data.Slaw.Internal.SlawIO
Description : Read and write binary slaw files
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module Data.Slaw.Internal.SlawIO
  ( SlawInputStream(..)
  , siRead
  , siClose
  , SlawOutputStream(..)
  , soWrite
  , soFlush
  , soClose
    --
  , openBinarySlawInput
  , withBinarySlawInput
  , readBinarySlawFile
    --
  , openBinarySlawOutput
  , withBinarySlawOutput
  , writeBinarySlawFile
    --
  , openBinarySlawInput1
  , fileMagic
  , binaryFileTypeSlaw
  , currentSlawVersion
  , readAllSlawx
  ) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as R
import qualified Data.ByteString.Lazy    as L
import Data.Default.Class
import Data.Hashable
import Data.Unique
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
import Data.Slaw.Internal.SlawEncode
-- import Data.Slaw.Internal.SlawPath
import Data.Slaw.Internal.SlawType
import Data.Slaw.Internal.Util

fileMagic :: Word32
fileMagic = 0xffff0b10

binaryFileTypeSlaw :: Word8
binaryFileTypeSlaw = 1

currentSlawVersion :: Word8
currentSlawVersion = 2

binTypeStr :: ByteOrder -> String
binTypeStr bo = "binary slaw file, " ++ endianName bo

endianName :: ByteOrder -> String
endianName BigEndian    = "big-endian"
endianName LittleEndian = "little-endian"

-- | A stream from which slawx can be read.
data SlawInputStream = SlawInputStream
  { siName   :: String -- ^ Get the name of the file we are reading from.
  , siType   :: String -- ^ Get a description of the file type.
  , siShow   :: String -- ^ Additional info for "show"
  , siUniq   :: Unique -- ^ Identity for Eq, Ord, and Hashable
  , siRead'  :: CallStack -> IO (Maybe Slaw)
  , siClose' :: CallStack -> IO ()
  }

instance Eq SlawInputStream where
  x == y = siUniq x == siUniq y

instance Ord SlawInputStream where
  x `compare` y = siUniq x ?? siUniq y

instance Hashable SlawInputStream where
  salt `hashWithSalt` x = salt ## hashUnique (siUniq x)

instance NFData SlawInputStream where
  rnf x = siName   x `deepseq`
          siType   x `deepseq`
          siShow   x `deepseq`
          siUniq   x `deepseq`
          siRead'  x `deepseq`
          rnf (siClose' x)

instance Show SlawInputStream where
  showsPrec _ x = showString "{SlawInputStream: "        .
                  showString (showEscapedStr $ siName x) .
                  showString " "                         .
                  showString (siShow x)                  .
                  showString "}"

-- | Read a 'Slaw' from the stream.  Returns 'Nothing' if
-- end-of-file has been reached.  If an error occurs, may
-- throw 'IOException' or t'PlasmaException'.
siRead :: HasCallStack => SlawInputStream -> IO (Maybe Slaw)
siRead si = siRead' si callStack

-- | Close the stream.
siClose :: HasCallStack => SlawInputStream -> IO ()
siClose si = siClose' si callStack

-- | A stream to which slawx can be written.
data SlawOutputStream = SlawOutputStream
  { soName   :: String -- ^ Get the name of the file we are writing to.
  , soType   :: String -- ^ Get a description of the file type.
  , soShow   :: String -- ^ Additional info for "show"
  , soUniq   :: Unique -- ^ Identity for Eq, Ord, and Hashable
  , soWrite' :: CallStack -> Slaw -> IO ()
  , soFlush' :: CallStack -> IO ()
  , soClose' :: CallStack -> IO ()
  }

instance Eq SlawOutputStream where
  x == y = soUniq x == soUniq y

instance Ord SlawOutputStream where
  x `compare` y = soUniq x ?? soUniq y

instance Hashable SlawOutputStream where
  salt `hashWithSalt` x = salt ## hashUnique (soUniq x)

instance NFData SlawOutputStream where
  rnf x = soName   x `deepseq`
          soType   x `deepseq`
          soShow   x `deepseq`
          soUniq   x `deepseq`
          soWrite' x `deepseq`
          soFlush' x `deepseq`
          rnf (soClose' x)

instance Show SlawOutputStream where
  showsPrec _ x = showString "{SlawOutputStream: "        .
                  showString (showEscapedStr $ soName x) .
                  showString " "                         .
                  showString (soShow x)                  .
                  showString "}"

-- | Write a 'Slaw' to the stream.
soWrite :: HasCallStack => SlawOutputStream -> Slaw -> IO ()
soWrite so = soWrite' so callStack

-- | Flush the stream (write any buffered data).
soFlush :: HasCallStack => SlawOutputStream -> IO ()
soFlush so = soFlush' so callStack

-- | Close the stream.
soClose :: HasCallStack => SlawOutputStream -> IO ()
soClose so = soClose' so callStack

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

-- | Opens a t'SlawInputStream' for reading from a binary slaw file.
-- If an error occurs, may throw 'IOException' or t'PlasmaException'.
--
-- Does not currently take any options, so the second argument is
-- placeholder which is just ignored.  The easiest thing to do
-- is just pass in @()@.
openBinarySlawInput :: (HasCallStack, FileClass a, ToSlaw b)
                    => a -- ^ name (or handle) of file to open
                    -> b -- ^ options map/protein (currently none)
                    -> IO SlawInputStream
openBinarySlawInput file opts = withFrozenCallStack $ do
  let nam = fcName file
  eth <- fcOpenReadOrMap file
  rdr <- makeFileReader eth
  openBinarySlawInput1 nam rdr opts

openBinarySlawInput1
  :: (HasCallStack, ToSlaw b)
  => String
  -> FileReader
  -> b
  -> IO SlawInputStream
openBinarySlawInput1 nam rdr _ = withFrozenCallStack $ do
  inp  <- makeSInput nam rdr
  uniq <- newUnique
  return $ SlawInputStream { siName   = nam
                           , siType   = binTypeStr (sinOrder inp)
                           , siShow   = "binary " ++ show (sinOrder inp)
                           , siUniq   = uniq
                           , siRead'  = readSInput  inp
                           , siClose' = closeSInput inp
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

closeSInput :: SInput -> CallStack -> IO ()
closeSInput inp _ = closeFileReader $ sinReader inp

-- | Run an action with a t'SlawInputStream'.
withBinarySlawInput
  :: (HasCallStack, FileClass a, ToSlaw b)
  => a                         -- ^ name (or handle) of file to read
  -> b                         -- ^ options map/protein (currently none)
  -> (SlawInputStream -> IO c) -- ^ action to run
  -> IO c
withBinarySlawInput fname opts act = withFrozenCallStack $ do
  bracket (openBinarySlawInput fname opts) siClose act

-- | Convenience function to read all the slawx from a binary
-- slaw file.  It opens a stream, reads all the slawx from the stream,
-- and then closes the stream.  The slawx that were read are returned
-- as a list.
readBinarySlawFile :: (HasCallStack, FileClass a, ToSlaw b)
                   => a -- ^ name (or handle) of file to read
                   -> b -- ^ options map/protein (currently none)
                   -> IO [Slaw]
readBinarySlawFile fname opts = withFrozenCallStack $ do
  withBinarySlawInput fname opts readAllSlawx

readAllSlawx :: SlawInputStream -> IO [Slaw]
readAllSlawx = readAllSlawx1 []

readAllSlawx1 :: [Slaw] -> SlawInputStream -> IO [Slaw]
readAllSlawx1 revSlawx sis = do
  mslaw <- siRead sis
  case mslaw of
    Nothing  -> return $ reverse revSlawx
    (Just s) -> readAllSlawx1 (s : revSlawx) sis

--

-- | Opens a t'SlawOutputStream' for writing to a binary slaw file.
-- If an error occurs, may throw 'IOException' or t'PlasmaException'.
--
-- The second argument is a map or protein which specifies options.
-- The easiest thing is to pass in t'WriteBinaryOptions' if you want
-- to specify any non-default options, or just pass @()@ to use
-- the defaults.
openBinarySlawOutput :: (FileClass a, ToSlaw b)
                     => a -- ^ name (or handle) of file to open
                     -> b -- ^ options map/protein
                     -> IO SlawOutputStream
openBinarySlawOutput file opts = do
  let opts' = toSlaw opts
      wbo   = ŝm     opts' ?> def
      nam   = fcName file
  (h, shouldClose) <- fcOpenWrite file
  out  <- makeSOutput nam (h, shouldClose) wbo
  uniq <- newUnique
  return $ SlawOutputStream { soName   = nam
                            , soType   = binTypeStr (soutOrder out)
                            , soShow   = "binary " ++ show (soutOrder out)
                            , soUniq   = uniq
                            , soWrite' = writeSOutput out
                            , soFlush' = flushSOutput out
                            , soClose' = closeSOutput out
                            }

getBo :: WriteBinaryOptions -> ByteOrder
getBo wbo = pbo2bo (wboByteOrder wbo ?> BoNative)

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
  autoFlush <- case wboAutoFlush wbo ?> AutoFlushIfNotSeekable of
                 AutoFlushNever         -> return False
                 AutoFlushAlways        -> return True
                 AutoFlushIfNotSeekable -> not <$> hIsSeekable h
  R.hPutBuilder h bld
  return $ SOutput { soutName   = nam
                   , soutOrder  = getBo wbo
                   , soutFlush  = autoFlush
                   , soutHandle = h
                   , soutClose  = shouldClose
                   }

writeSOutput :: SOutput -> CallStack -> Slaw -> IO ()
writeSOutput sout _ s = do
  let bo   = soutOrder           sout
      af   = soutFlush           sout
      h    = soutHandle          sout
      bld  = encodeSlawToBuilder bo s
  R.hPutBuilder h bld
  if af
    then hFlush h
    else return ()

flushSOutput :: SOutput -> CallStack -> IO ()
flushSOutput sout _ = hFlush $ soutHandle sout

closeSOutput :: SOutput -> CallStack -> IO ()
closeSOutput sout _ =
  let h = soutHandle sout
  in if soutClose sout
     then hClose h
     else hFlush h

-- | Run an action with a t'SlawOutputStream'.
withBinarySlawOutput
  :: (FileClass a, ToSlaw b)
  => a                          -- ^ name (or handle) of file to read
  -> b                          -- ^ options map/protein
  -> (SlawOutputStream -> IO c) -- ^ action to run
  -> IO c
withBinarySlawOutput fname opts act = do
  bracket (openBinarySlawOutput fname opts) soClose act

-- | Convenience function to write a binary slaw file all at once.
-- It opens a stream, writes all the slawx to the stream,
-- and then closes the stream.
writeBinarySlawFile :: (FileClass a, ToSlaw b)
                    => a -- ^ name (or handle) of file to write
                    -> b -- ^ options map/protein
                    -> [Slaw] -- ^ slawx to write to file
                    -> IO ()
writeBinarySlawFile fname opts ss = do
  withBinarySlawOutput fname opts $ \sos -> mapM_ (soWrite sos) ss
