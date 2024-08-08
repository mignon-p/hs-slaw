{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE RankNTypes                 #-}

module TestUtil
  ( AssEqFunc
  , IoFunc
  , PropIO
  , fpHU
  , fpQC
  , roundTripIOwr
  , roundTripIOrw
  , checkSlawRead
  , checkSlawWrite
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
-- import qualified Test.QuickCheck          as QC
import qualified Test.QuickCheck.Monadic  as QC

import Data.Slaw
import Data.Slaw.IO

type AssEqFunc m = forall a. (HasCallStack, Eq a, Show a) => String -> a -> a -> m ()
type IoFunc    m = forall a. IO a -> m a
type FuncPair  m = (AssEqFunc m, IoFunc m)
type PropIO      = QC.PropertyM IO

fpHU :: FuncPair IO
fpHU = (assertEqual, id)

fpQC :: FuncPair PropIO
fpQC = (qcAssEq, QC.run)

qcAssEq :: AssEqFunc PropIO
qcAssEq msg x y = do
  let msg' = msg ++ ": " ++ show x ++ " ≠ " ++ show y
  QC.assertWith (x == y) msg'

tmpDir :: FilePath
tmpDir = unsafePerformIO getTemporaryDirectory

roundTripIOwr :: (HasCallStack, Monad m)
              => FuncPair m
              -> [Slaw]
              -> WriteBinaryOptions
              -> Bool
              -> m ()
roundTripIOwr (assEq, io) ss wbo useName = do
  (fname, h) <- io $ openBinaryTempFile tmpDir "test.slaw"
  io $ if useName
       then writeBinarySlawFile          h  wbo ss
       else writeBinarySlawFile (NoClose h) wbo ss

  ss' <- io $ if useName
              then readBinarySlawFile fname ()
              else hSeek h AbsoluteSeek 0 >> readBinarySlawFile h ()
  io $ removeFile fname

  let len  = length ss
      len' = length ss'
      pfx1 = concat [", useName = ", show useName, ", ", show wbo]
  assEq ("length" ++ pfx1) len len'

  forM_ (zip3 ss ss' [0..]) $ \(s, s', i) -> do
    let pfx = concat ["slaw #", show (i :: Int), pfx1]
    assEq pfx s s'

roundTripIOrw :: (HasCallStack, Monad m)
              => FuncPair m
              -> FilePath
              -> FilePath
              -> ByteOrder
              -> m ()
roundTripIOrw (assEq, io) orig expected bo = do
  ss  <- io $ readBinarySlawFile orig ()

  let wbo = def { wboByteOrder = bo2pbo bo }
  (fname, h) <- io $ openBinaryTempFile   tmpDir "test.slaw"
  io $ writeBinarySlawFile h wbo ss

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

checkSlawRead :: HasCallStack
              => FilePath
              -> [Slaw]
              -> IO ()
checkSlawRead fname ss = do
  ss' <- readBinarySlawFile fname ()
  let nExpected = length ss
      nActual   = length ss'
  assertEqual (fname ++ ":length") nExpected nActual

  forM_ (zip3 ss ss' [0..]) $ \(s, s', i) -> do
    let pfx = fname ++ ":slaw #" ++ show (i :: Int)
    assertEqual pfx s s'

checkSlawWrite :: HasCallStack
               => FilePath
               -> [Slaw]
               -> ByteOrder
               -> IO ()
checkSlawWrite = undefined
