-- import Data.List
import System.Environment
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import Data.Slaw.Internal.SlawDecode
import Data.Slaw.Internal.SlawEncode
-- import Data.Slaw.Internal.SlawType
-- import Data.Slaw.Internal.Util

import SlawInstances ()

main :: IO ()
main = do
  setIfNotSet "TASTY_COLOR" "always"
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProps]

setIfNotSet :: String -> String -> IO ()
setIfNotSet var val = do
  old <- lookupEnv var
  case old of
    Just (_:_) -> return ()
    _          -> setEnv var val

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "round-trip (big endian)" $
      \slaw -> slaw QC.=== decodeSlaw BigEndian (encodeSlaw BigEndian slaw)
  , QC.testProperty "round-trip (little endian)" $
      \slaw -> slaw QC.=== decodeSlaw LittleEndian (encodeSlaw LittleEndian slaw)
  ]
