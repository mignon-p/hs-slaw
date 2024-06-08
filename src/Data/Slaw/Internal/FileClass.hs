{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Data.Slaw.Internal.FileClass
  ( FileClass(..)
  ) where

import Control.Exception
import qualified Data.ByteString           as B
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
