{-|
Module      : Data.Slaw.Internal.FileClass
Description : Class to open files by either FilePath or OsPath
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Data.Slaw.Internal.FileClass
  ( FileClass(..)
  , FileReader(..)
  , NoClose(..)
  , HPair
  , makeFileReader
  , readBytes
  , peekBytes
  , closeFileReader
  , getOffset
  ) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as L
import Data.IORef
import Data.List
import qualified System.File.OsPath        as F
import System.IO
import System.IO.MMap
import System.OsPath

import Data.Slaw.Internal.Util

-- | A 'Handle', and a boolean indicating whether we should
-- close the 'Handle' when we're done with it.
type HPair = (Handle, Bool)

-- | Represents a filename (as either a 'FilePath' or an 'OsPath')
-- or an already-open 'Handle'.
class FileClass a where
  -- | Returns the name of the file.
  fcName              :: a -> String

  -- | Opens the file for reading, as a 'Handle'.
  fcOpenRead          :: a -> IO HPair

  -- | Opens the file for writing, as a 'Handle'.
  fcOpenWrite         :: a -> IO HPair

  -- | Memory-maps the entire file (read-only) if possible,
  -- and returns it as a single 'B.ByteString' (which is backed
  -- by the mapped memory).  If the file cannot be mapped,
  -- opens it for reading and returns a 'Handle'.
  fcOpenReadOrMap     :: a -> IO (Either B.ByteString HPair)
  fcOpenReadOrMap name = Right <$> fcOpenRead name

instance FileClass FilePath where
  fcName           = id
  fcOpenRead  name = (,True) <$> openBinaryFile name ReadMode
  fcOpenWrite name = (,True) <$> openBinaryFile name WriteMode

  fcOpenReadOrMap name = do
    mbs <- mmapMaybe name
    case mbs of
      Just bs -> return $ Left bs
      Nothing -> Right <$> fcOpenRead name

instance FileClass OsPath where
  fcName name =
    case decodeUtf name of
      Just str -> str
      Nothing  -> show name

  fcOpenRead  name = (,True) <$> F.openBinaryFile name ReadMode
  fcOpenWrite name = (,True) <$> F.openBinaryFile name WriteMode

  fcOpenReadOrMap name = do
    mbs <- case decodeUtf name of
             Nothing  -> return Nothing
             Just str -> mmapMaybe str
    case mbs of
      Just bs -> return $ Left bs
      Nothing -> Right <$> fcOpenRead name

instance FileClass Handle where
  fcName        = nameFromHandle
  fcOpenRead  h = return (h, True)
  fcOpenWrite h = return (h, True)

-- | Wraps a 'Handle' and indicates that the handle should be
-- left open even when the stream that uses the handle is closed.
-- This is useful for handles like 'stdin' and 'stdout'.
newtype NoClose = NoClose { unNoClose :: Handle }
                  deriving newtype (Show, Eq)

instance FileClass NoClose where
  fcName      (NoClose h) = nameFromHandle h
  fcOpenRead  (NoClose h) = return (h, False)
  fcOpenWrite (NoClose h) = return (h, False)

mmapMaybe :: FilePath -> IO (Maybe B.ByteString)
mmapMaybe name = do
  eth <- try $ mmapFileByteString name Nothing
  case (eth :: Either IOException B.ByteString) of
    Right bs
      | not (B.null bs) -> return $ Just bs
    _                   -> return   Nothing

-- We want to extract the filename portion:
--   "{handle: /dev/null}" -> "/dev/null"
--   "{handle: <stderr>}"  -> "<stderr>"
nameFromHandle :: Handle -> String
nameFromHandle h =
  let hname    = show h
      pfx      = "{handle: "
      sfx      = "}"
      pfxLen   = length pfx
      sfxLen   = length sfx
      hnameLen = length hname
      nameLen  = hnameLen - (pfxLen + sfxLen)
  in if pfx `isPrefixOf` hname && sfx `isSuffixOf` hname
     then take nameLen $ drop pfxLen hname
     else hname

--

-- | A handy facility for reading from a file, with the following
-- features:
--
-- * May be either a 'Handle' or a 'B.ByteString'.
--
-- * Supports peeking an arbitrary number of bytes from the
--   file, as well as reading.
--
-- * Keeps track of the byte offset within the file.
data FileReader = FileReader
  { frBytes  :: IORef L.ByteString
  , frHandle :: Maybe HPair
  , frOffset :: IORef Integer
  }

makeFileReader :: Either B.ByteString HPair -> IO FileReader
makeFileReader (Left bs) = do
  r <- newIORef $ L.fromStrict bs
  o <- newIORef 0
  return $ FileReader r Nothing o
makeFileReader (Right h) = do
  eth <- tryIO $ hTell $ fst h
  let offset = case eth of
                 Left  _ -> 0
                 Right x -> x
  r <- newIORef $ L.empty
  o <- newIORef offset
  return $ FileReader r (Just h) o

readBytes :: FileReader -> Int -> IO L.ByteString
readBytes fr nBytes = do
  lbs <- readBytes' fr nBytes
  let len = fromIntegral $ L.length lbs
  modifyIORef' (frOffset fr) (+ len)
  return lbs

readBytes' :: FileReader -> Int -> IO L.ByteString
readBytes' fr nBytes = do
  lbs1 <- readIORef $ frBytes fr
  let (lbs2, lbs3) = L.splitAt (fromIntegral nBytes) lbs1
  when (not $ L.null lbs1) $ writeIORef (frBytes fr) lbs3
  let n = L.length lbs2
  if n >= fromIntegral nBytes
    then return lbs2
    else case frHandle fr of
           Nothing     -> return lbs2
           Just (h, _) -> do
             let bytesLeft = nBytes - fromIntegral n
             lbs4 <- L.hGet h bytesLeft
             return $ lbs2 <> lbs4

peekBytes :: FileReader -> Int -> IO L.ByteString
peekBytes fr nBytes = do
  lbs1 <- readIORef $ frBytes fr
  let lbs2 = L.take (fromIntegral nBytes) lbs1
  if L.length lbs2 >= fromIntegral nBytes
    then return lbs2
    else do
    lbs4 <- readBytes' fr nBytes
    modifyIORef' (frBytes fr) (lbs4 <>)
    return lbs4

closeFileReader :: FileReader -> IO ()
closeFileReader fr = do
  writeIORef (frBytes fr) L.empty
  case frHandle fr of
    Just (h, True) -> hClose h
    _              -> return ()

getOffset :: FileReader -> IO Integer
getOffset = readIORef . frOffset
