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
import Data.Word
import qualified System.File.OsPath        as F
import System.IO
import System.IO.MMap
import System.OsPath

type HPair = (Handle, Bool)

class FileClass a where
  fcName              :: a -> String

  fcOpenRead          :: a -> IO HPair

  fcOpenWrite         :: a -> IO HPair

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
  fcName        = show
  fcOpenRead  h = return (h, True)
  fcOpenWrite h = return (h, True)

newtype NoClose = NoClose { unNoClose :: Handle }
                  deriving newtype (Show, Eq)

instance FileClass NoClose where
  fcName      (NoClose h) = show h
  fcOpenRead  (NoClose h) = return (h, False)
  fcOpenWrite (NoClose h) = return (h, False)

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
  , frHandle :: Maybe HPair
  , frOffset :: IORef Word64
  }

makeFileReader :: Either B.ByteString HPair -> IO FileReader
makeFileReader (Left bs) = do
  r <- newIORef $ L.fromStrict bs
  o <- newIORef 0
  return $ FileReader r Nothing o
makeFileReader (Right h) = do
  r <- newIORef $ L.empty
  o <- newIORef 0
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

getOffset :: FileReader -> IO Word64
getOffset = readIORef . frOffset
