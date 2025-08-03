{-# LANGUAGE CPP                        #-}
{-# LANGUAGE OverloadedStrings          #-}

import Control.Exception
import Data.List
import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.Utils
import Distribution.Types.BuildInfo
import Distribution.Types.LocalBuildInfo
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Verbosity
import System.IO
import System.IO.Error

#if defined(VERSION_Cabal) && MIN_VERSION_Cabal(3,14,0)
#define FWS_CWD Nothing
#else
#define FWS_CWD
#endif

main :: IO ()
main = defaultMainWithHooks hooks

hooks :: UserHooks
hooks = simpleUserHooks { hookedPreProcessors = [ suffixHandler ] }

suffixHandler :: PPSuffixHandler
suffixHandler = ("hs-template", makePreprocessor)

makePreprocessor :: BuildInfo
                 -> LocalBuildInfo
                 -> ComponentLocalBuildInfo
                 -> PreProcessor
makePreprocessor _ _ _ = ppUnlit { runPreProcessor = runPP }
  where runPP = mkSimplePreProcessor processTemplate

processTemplate :: FilePath -> FilePath -> Verbosity -> IO ()
processTemplate inFile outFile verbosity = do
  mScript <- findScript inFile
  case mScript of
    Nothing -> do
      let msg = concat [ "Could not find "
                       , scriptRel
                       , " to preprocess "
                       , inFile
                       ]
      die' verbosity msg
    Just script -> do
      let msg = concat [ "Preprocessing "
                       , inFile
                       , " to "
                       , outFile
                       , " with "
                       , script
                       ]
      info verbosity msg
      rawSystemExit verbosity FWS_CWD "perl" [script, inFile, outFile]

parentDir :: FilePath -> FilePath
parentDir = dropWhileEnd (/= '/') . dropWhileEnd (== '/')

checkIfFileExists :: FilePath -> IO Bool
checkIfFileExists fname = do
  eth <- tryIoe $ withFile fname ReadMode (return . (const ()))
  case eth of
    Left  _ -> return False
    Right _ -> return True

tryIoe :: IO a -> IO (Either IOError a)
tryIoe = try

scriptRel :: FilePath
scriptRel = "scripts/preprocess-template.pl"

findScript :: FilePath -> IO (Maybe FilePath)
findScript ""    = return Nothing
findScript child = do
  let dir    = parentDir child
      script = dir ++ scriptRel
  exists <- checkIfFileExists script
  if exists
    then return $ Just script
    else findScript dir
