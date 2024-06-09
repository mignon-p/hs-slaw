{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Data.Slaw.Internal.FileClass
  ( FileClass(..)
  , FileReader(..)
  , makeFileReader
  , readBytes
  , peekBytes
  , closeFileReader
  ) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as L
import Data.IORef
import qualified System.File.OsPath        as F
import System.IO
import System.IO.MMap
import System.OsPath

class FileClass a where
  fcName              :: a -> String

  fcOpenRead          :: a -> IO Handle

  fcOpenWrite         :: a -> IO Handle

  fcOpenReadOrMap     :: a -> IO (Either B.ByteString Handle)
  fcOpenReadOrMap name = Right <$> fcOpenRead name

instance FileClass FilePath where
  fcName           = id
  fcOpenRead  name = openBinaryFile name ReadMode
  fcOpenWrite name = openBinaryFile name WriteMode

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

  fcOpenRead  name = F.openBinaryFile name ReadMode
  fcOpenWrite name = F.openBinaryFile name WriteMode

  fcOpenReadOrMap name = do
    mbs <- case decodeUtf name of
             Nothing  -> return Nothing
             Just str -> mmapMaybe str
    case mbs of
      Just bs -> return $ Left bs
      Nothing -> Right <$> fcOpenRead name

instance FileClass Handle where
  fcName      = show
  fcOpenRead  = return
  fcOpenWrite = return

mmapMaybe :: FilePath -> IO (Maybe B.ByteString)
mmapMaybe name = do
  eth <- try $ mmapFileByteString name Nothing
  case (eth :: Either IOException B.ByteString) of
    Right bs
      | not (B.null bs) -> return $ Just bs
    _                   -> return   Nothing

--

data FileReader = FileReader
  { frBytes  :: IORef L.ByteString
  , frHandle :: Maybe Handle
  }

makeFileReader :: Either B.ByteString Handle -> IO FileReader
makeFileReader (Left bs) = do
  r <- newIORef $ L.fromStrict bs
  return $ FileReader r Nothing
makeFileReader (Right h) = do
  r <- newIORef $ L.empty
  return $ FileReader r (Just h)

readBytes :: FileReader -> Int -> IO L.ByteString
readBytes fr nBytes = do
  lbs1 <- readIORef $ frBytes fr
  let (lbs2, lbs3) = L.splitAt (fromIntegral nBytes) lbs1
  when (not $ L.null lbs1) $ writeIORef (frBytes fr) lbs3
  let n = L.length lbs2
  if n >= fromIntegral nBytes
    then return lbs2
    else case frHandle fr of
           Nothing -> return lbs2
           Just h  -> do
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
    lbs4 <- readBytes fr nBytes
    modifyIORef' (frBytes fr) (lbs4 <>)
    return lbs4

closeFileReader :: FileReader -> IO ()
closeFileReader fr = do
  writeIORef (frBytes fr) L.empty
  case frHandle fr of
    Nothing -> return ()
    Just h  -> hClose h
