{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE RankNTypes                 #-}

module TestUtil
  ( AssEqFunc
  , IoFunc
  , roundTripIOwr
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
-- import Test.Tasty.HUnit

import Data.Slaw
import Data.Slaw.IO

type AssEqFunc m = forall a. (HasCallStack, Eq a, Show a) => String -> a -> a -> m ()
type IoFunc m = forall a. IO a -> m a

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

roundTripIOwr :: (HasCallStack, Monad m)
              => (AssEqFunc m, IoFunc m)
              -> [Slaw]
              -> WriteBinaryOptions
              -> Bool
              -> m ()
roundTripIOwr (assEq, io) ss wbo useName = do
  (fname, h) <- io $ openBinaryTempFile tmpDir "test.slaw"
  sos <- io $ if useName
              then openBinarySlawOutput h wbo
              else openBinarySlawOutput (NoClose h) wbo
  io $ do
    mapM_ (soWrite sos) ss
    soClose sos

  sis <- io $ if useName
              then openBinarySlawInput fname ()
              else hSeek h AbsoluteSeek 0 >> openBinarySlawInput h ()
  ss' <- io $ readAllSlawx sis
  io $ do
    siClose sis
    removeFile fname

  let len  = length ss
      len' = length ss'
      pfx1 = concat [", useName = ", show useName, ", ", show wbo]
  assEq ("length" ++ pfx1) len len'

  forM_ (zip3 ss ss' [0..]) $ \(s, s', i) -> do
    let pfx = concat ["slaw #", show (i :: Int), pfx1]
    assEq pfx s s'

roundTripIOrw :: (HasCallStack, Monad m)
              => (AssEqFunc m, IoFunc m)
              -> FilePath
              -> FilePath
              -> PreferredByteOrder
              -> m ()
roundTripIOrw (assEq, io) orig expected pbo = do
  sis <- io $ openBinarySlawInput orig ()
  ss  <- io $ readAllSlawx sis
  io $ siClose sis

  let wbo = def { wboByteOrder = pbo }
  (fname, h) <- io $ openBinaryTempFile   tmpDir "test.slaw"
  sos        <- io $ openBinarySlawOutput h      wbo
  io $ do
    mapM_      (soWrite sos) ss
    soClose    sos

  bsExpected <- io $ B.readFile expected
  bsActual   <- io $ B.readFile fname
  io $ removeFile fname

  let lenExpected = B.length bsExpected
      lenActual   = B.length bsActual
  assEq (expected ++ ":length") lenExpected lenActual

  let expW8 = B.unpack bsExpected
      actW8 = B.unpack bsActual
  forM_ (zip3 expW8 actW8 [0..]) $ \(e8, a8, i) -> do
    let pfx = concat [ expected
                     , ":#"
                     , show (i :: Int)
                     ]
    assEq pfx e8 a8
