module TestUtil
  ( roundTripIO
  ) where

import Control.Monad
import System.Directory
-- import System.Environment
import System.IO
import System.IO.Unsafe
-- import Test.Tasty
import Test.Tasty.HUnit

import Data.Slaw
import Data.Slaw.IO

tmpDir :: FilePath
tmpDir = unsafePerformIO getTemporaryDirectory

readAllSlawx :: SlawInputStream -> IO [Slaw]
readAllSlawx = readAllSlawx1 []

readAllSlawx1 :: [Slaw] -> SlawInputStream -> IO [Slaw]
readAllSlawx1 revSlawx sis = do
  mslaw <- siRead sis
  case mslaw of
    Nothing  -> return $ reverse revSlawx
    (Just s) -> readAllSlawx1 (s : revSlawx) sis

roundTripIO :: [Slaw] -> WriteBinaryOptions -> Bool -> IO ()
roundTripIO ss wbo useName = do
  (fname, h) <- openBinaryTempFile tmpDir "test.slaw"
  sos <- if useName
         then openBinarySlawOutput h wbo
         else openBinarySlawOutput (NoClose h) wbo
  mapM_ (soWrite sos) ss
  soClose sos

  sis <- if useName
         then openBinarySlawInput fname ()
         else hSeek h AbsoluteSeek 0 >> openBinarySlawInput h ()
  ss' <- readAllSlawx sis
  siClose sis
  removeFile fname

  let len  = length ss
      len' = length ss'
  len @=? len'

  forM_ (zip3 ss ss' [0..]) $ \(s, s', i) -> do
    let pfx = concat [ "slaw #"
                     , show (i :: Int)
                     , ", useName = "
                     , show useName
                     , ", "
                     , show wbo
                     ]
    assertEqual pfx s s'
