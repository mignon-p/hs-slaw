module TestUtil
  ( roundTripIOwr
  , roundTripIOrw
  ) where

import Control.Monad
import qualified Data.ByteString          as B
-- import qualified Data.ByteString.Lazy     as L
import Data.Default.Class
import GHC.Stack
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

roundTripIOwr :: HasCallStack
              => [Slaw]
              -> WriteBinaryOptions
              -> Bool
              -> IO ()
roundTripIOwr ss wbo useName = do
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
      pfx1 = concat [", useName = ", show useName, ", ", show wbo]
  assertEqual ("length" ++ pfx1) len len'

  forM_ (zip3 ss ss' [0..]) $ \(s, s', i) -> do
    let pfx = concat ["slaw #", show (i :: Int), pfx1]
    assertEqual pfx s s'

roundTripIOrw :: HasCallStack
              => FilePath
              -> FilePath
              -> PreferredByteOrder
              -> IO ()
roundTripIOrw orig expected pbo = do
  sis <- openBinarySlawInput orig ()
  ss  <- readAllSlawx sis
  siClose sis

  let wbo = def { wboByteOrder = pbo }
  (fname, h) <- openBinaryTempFile   tmpDir "test.slaw"
  sos        <- openBinarySlawOutput h      wbo
  mapM_      (soWrite sos) ss
  soClose    sos

  bsExpected <- B.readFile expected
  bsActual   <- B.readFile fname
  removeFile fname

  let lenExpected = B.length bsExpected
      lenActual   = B.length bsActual
  assertEqual (expected ++ ":length") lenExpected lenActual

  let expW8 = B.unpack bsExpected
      actW8 = B.unpack bsActual
  forM_ (zip3 expW8 actW8 [0..]) $ \(e8, a8, i) -> do
    let pfx = concat [ expected
                     , ":#"
                     , show (i :: Int)
                     ]
    assertEqual pfx e8 a8
