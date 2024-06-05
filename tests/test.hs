import qualified Data.ByteString.Lazy as L
-- import Data.List
import System.Environment
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import Data.Slaw

import SlawInstances ()

main :: IO ()
main = do
  setIfNotSet "TASTY_COLOR" "always"
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProps, unitTests]

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
  , QC.testProperty "Slaw addition" $
      \(x, y) -> (x :: Integer) + (y :: Integer) QC.=== ŝ (š x + š y)
  , QC.testProperty "Slaw subtraction" $
      \(x, y) -> (x :: Integer) - (y :: Integer) QC.=== ŝ (š x - š y)
  , QC.testProperty "Slaw multiplication" $
      \(x, y) -> (x :: Integer) * (y :: Integer) QC.=== ŝ (š x * š y)
  , QC.testProperty "Slaw negation" $
      \x -> (-x :: Integer) QC.=== ŝ (-š x)
  , QC.testProperty "Slaw absolute value" $
      \x -> abs (x :: Integer) QC.=== ŝ (abs (š x))
  , QC.testProperty "Slaw signum" $
      \x -> signum (x :: Integer) QC.=== ŝ (signum (š x))
  ]

unitTests :: TestTree
unitTests = testGroup "HUnit tests"
  [ testCase "Error slaw (big endian)"    $ testErrorSlaw BigEndian
  , testCase "Error slaw (little endian)" $ testErrorSlaw LittleEndian
  ]

testErrorSlaw :: ByteOrder -> Assertion
testErrorSlaw bo = do
  let zeros = L.replicate 80 0
      s1    = decodeSlaw bo zeros
      bin1  = encodeSlaw bo s1
      s2    = decodeSlaw bo bin1
  assertBool "isError s1" $ isError s1
  assertBool "isError s2" $ isError s2

isError :: Slaw -> Bool
isError (SlawError _ _) = True
isError _               = False
