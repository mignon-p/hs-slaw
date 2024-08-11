{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE RankNTypes                 #-}

import Control.Monad
import qualified Data.ByteString.Lazy     as L
import Data.Complex
import Data.Default.Class
import Data.Either
import Data.Int
import qualified Data.IntMap.Strict       as IM
-- import Data.List
import qualified Data.Map.Strict          as M
import qualified Data.Text                as T
import qualified Data.Vector              as V
import qualified Data.Vector.Storable     as S
import Data.Word
-- import Numeric.Half
-- import System.Directory
import System.Environment
-- import System.IO
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck    as QC
import qualified Test.QuickCheck.Monadic  as QC

import Data.Slaw
import Data.Slaw.IO
import Data.Slaw.Path
import Data.Slaw.Semantic

import Comprehensive
import SlawInstances ()
import TestUtil

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
qcProps = testGroup "QuickCheck tests"
  [ QC.testProperty "round-trip (big endian)" $
      \slaw -> slaw QC.=== decodeSlaw BigEndian (encodeSlaw BigEndian slaw)
  , QC.testProperty "round-trip (little endian)" $
      \slaw -> slaw QC.=== decodeSlaw LittleEndian (encodeSlaw LittleEndian slaw)
  , QC.testProperty "decodeSlawLength" $
      \(slaw, bo) ->
        let enc      = encodeSlaw bo slaw
            expected = Right $ fromIntegral $ L.length $ enc
        in expected QC.=== decodeSlawLength bo enc
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
  , QC.testProperty "Slaw validation" $
      \x -> Right () QC.=== validateSlaw [] x
  , QC.testProperty "round-trip IO (big endian)"    $ rtIoProp BigEndian
  , QC.testProperty "round-trip IO (little endian)" $ rtIoProp LittleEndian
  ]

unitTests :: TestTree
unitTests = testGroup "HUnit tests"
  [ testCase "Error slaw (big endian)"    $ testErrorSlaw BigEndian
  , testCase "Error slaw (little endian)" $ testErrorSlaw LittleEndian
  , testCase "slaw-path"                  $ testSlawPath
  , testCase "slaw-convert"               $ testSlawConvert
  , testCase "slaw-semantic"              $ testSlawSemantic
  , testCase "slaw-validation"            $ testSlawValidation
  , testCase "slaw-monoid"                $ testSlawMonoid
  , testCase "slaw-io"                    $ testSlawIO
  ]

rtIoProp :: ByteOrder -> Slaw -> QC.Property
rtIoProp bo s = QC.monadicIO $ do
  let wbo = def { wboByteOrder = bo2pbo bo }
  roundTripIOwr fpQC [s] wbo False

testErrorSlaw :: ByteOrder -> Assertion
testErrorSlaw bo = do
  let zeros = L.replicate 80 0
      s1    = decodeSlaw bo zeros
      bin1  = encodeSlaw bo s1
      s2    = decodeSlaw bo bin1
  assertBool "isError s1" $ isError s1
  assertBool "isError s2" $ isError s2

mySlaw :: Slaw
mySlaw = š $ protein "have an ice day"
  [ ("vaults",  š vaults)
  , ("pair",    SlawCons "foo" "bar")
  , ("vectors", š (S.fromList vectors))
  ]

vaults :: IM.IntMap (M.Map T.Text Slaw)
vaults = IM.fromList
  [ (33, M.fromList [ ("status",    "good")
                    , ("neighbors", š $ S.fromList [31, 32 :: Int8])
                    ])
  , (32, M.fromList [ ("status",    "destroyed")
                    , ("neighbors", š $ S.fromList [31, 33 :: Int8])
                    ])
  , (31, M.fromList [ ("status",    "fake")
                    , ("neighbors", š $ S.fromList [32, 33 :: Int8])
                    ])
  ]

vectors :: [V4 (Complex Double)]
vectors =
  [ V4 (1 :+ 2) (3 :+ 4) (5 :+ 6) (7 :+ 8)
  , V4 (9 :+ 8) (7 :+ 6) (5 :+ 4) (3 :+ 2)
  , V4 0 0 0 0
  , V4 11 12 13 14
  ]

exampleSlawx :: [Slaw]
exampleSlawx =
  [ š (5 :: Word64)
  , š ("stuff" :: String, (-3) :: Int8)
  , š (V4 0 0 0 (0 :: Float))
  , mySlaw ! "pair"
  , slawPath fv mySlaw "descrips"
  , mySlaw ! "vaults/33/neighbors"
  ]
  where fv = def { spoProteinMode = PmFullyVisible }

testSlawPath :: Assertion
testSlawPath = do
  let sp    = slawPath_m spo   mySlaw
      spCI  = slawPath_m spoCI mySlaw
      jstr  = Just . T.unpack
      nf    = def { nfArray = True }
      nd    = NumUnt8 S.empty
      spoCI = def   { spoProteinMode     = PmFullyVisible }
      spo   = spoCI { spoCaseInsensitive = False          }

  Just "have" @=? sp "des/0"
  Just "an"   @=? sp "des/1"
  Just "ice"  @=? sp "des/2"
  Just "day"  @=? sp "des/3"
  Nothing     @=? sp "des/4"

  Just "foo"  @=? sp "ing/pair/car"
  Just "bar"  @=? sp "ing/pair/cdr"

  -- spoCaseInsensitive only affects map lookup.
  -- special keywords like "ing" and "cdr" are always case insensitive.
  Just "foo"  @=? sp   "iNg/pair/car"
  Just "bar"  @=? sp   "ing/pair/cDr"

  Just "foo"  @=? spCI "ing/paiR/car"
  Just "bar"  @=? spCI "ing/pAir/cdr"

  Nothing     @=? sp   "ing/paiR/car"
  Nothing     @=? sp   "ing/pAir/cdr"

  Just (SlawNumeric nf nd) @=? sp "rude"

  jstr "fake"      @=? mySlaw !? "vaults/31/status"
  jstr "destroyed" @=? mySlaw !? "vaults/32/statUs"
  jstr "good"      @=? mySlaw !? "vauLts/33/status"

  Just (32 :: Int) @=? mySlaw !? "vaults/31/neighbors/0"
  Just (33 :: Int) @=? mySlaw !? "vaults/31/neighbors/1"

  Just (31 :: Int) @=? mySlaw !? "vaults/32/neighbors/0"
  Just (33 :: Int) @=? mySlaw !? "vaults/32/neighbors/1"

  Just (31 :: Int) @=? mySlaw !? "vaults/33/neighbors/0"
  Just (32 :: Int) @=? mySlaw !? "vaults/33/neighbors/1"

  Just (1 :: Double) @=? mySlaw !? "vectors/0/x/re"
  Just (2 :: Double) @=? mySlaw !? "vectors/0/X/im"
  Just (3 :: Double) @=? mySlaw !? "vectors/0/y/re"
  Just (4 :: Double) @=? mySlaw !? "vectors/0/y/im"
  Just (5 :: Double) @=? mySlaw !? "vectors/0/Z/re"
  Just (6 :: Double) @=? mySlaw !? "vectors/0/z/im"
  Just (7 :: Double) @=? mySlaw !? "vectors/0/w/re"
  Just (8 :: Double) @=? mySlaw !? "vectors/0/w/im"

  Semantic 9 @=? Semantic (mySlaw ! "vectors/1/x/re")
  Semantic 8 @=? Semantic (mySlaw ! "vectors/1/x/im")
  Semantic 7 @=? Semantic (mySlaw ! "vectors/1/y/RE")
  Semantic 6 @=? Semantic (mySlaw ! "vectors/1/y/im")
  Semantic 5 @=? Semantic (mySlaw ! "vectors/1/z/re")
  Semantic 4 @=? Semantic (mySlaw ! "vectors/1/z/IM")
  Semantic 3 @=? Semantic (mySlaw ! "vectors/1/w/re")
  Semantic 2 @=? Semantic (mySlaw ! "Vectors/1/w/im")

  (Nothing :: Maybe Double) @=? mySlaw !? "vectors/0/w/banana"
  Just (14 :: Double)       @=? mySlaw !? "vectors/-1/-1/0"

  SemanticCI "FOO" @=? SemanticCI (mySlaw ! "pair/0")
  SemanticCI "bAr" @=? SemanticCI (mySlaw ! "Pair/1")

testSlawConvert :: Assertion
testSlawConvert = do
  Just False @=? ŝm "fAlse"
  Just True  @=? ŝm "truE"
  Just False @=? ŝm "off"
  Just True  @=? ŝm "on"
  Just False @=? ŝm "no"
  Just True  @=? ŝm "yes"
  Just False @=? ŝm "0"
  Just True  @=? ŝm "1"
  Just False @=? ŝm 0
  Just True  @=? ŝm 1
  Just True  @=? ŝm (-5)
  Just True  @=? ŝm 0xdefacedbadfacade

  let s = SlawList ["one", "two", 3]
      v = ŝ s :: V.Vector T.Text

  Just "one" @=? v V.!? 0
  Just "two" @=? v V.!? 1
  Just "3"   @=? v V.!? 2

testSlawSemantic :: Assertion
testSlawSemantic = do
  assertBool "[0]" $ š (5 :: Int8) ==~ š (5 :: Word64)
  assertBool "[1]" $ not $ "foo" ==~  "Foo"
  assertBool "[2]" $       "foo" ==~~ "Foo"

valSlaw :: [ValidationFlag] -> Slaw -> Bool
valSlaw vf = isRight . validateSlaw vf

testSlawValidation :: Assertion
testSlawValidation = do
  let halfNd    = NumFloat16 $ S.fromList [0.0, 1.0, pi]
      halfNum   = SlawNumeric arrNf halfNd
      badUtf8   = SlawString $ L.pack [0x40, 0xff, 0x20]
      badDesIng = SlawProtein (Just halfNum) (Just badUtf8) mempty
      goodSym   = SlawSymbol 37619
      badSym1   = SlawSymbol 0                  -- reserved
      badSym2   = SlawSymbol 0xde881fb33b78164e -- too large
      badNf     = NumericFormat { nfArray   = True
                                , nfComplex = True
                                , nfVector  = Vt5mv
                                }
      singNf    = NumericFormat { nfArray   = False
                                , nfComplex = False
                                , nfVector  = VtScalar
                                }
      arrNf     = NumericFormat { nfArray   = True
                                , nfComplex = False
                                , nfVector  = VtScalar
                                }
      cplxNf    = NumericFormat { nfArray   = True
                                , nfComplex = True
                                , nfVector  = VtScalar
                                }
      emptyNd   = NumFloat64 $ S.fromList []
      emptyNum1 = SlawNumeric arrNf  emptyNd
      emptyNum2 = SlawNumeric cplxNf emptyNd
      badNum1   = SlawNumeric badNf  emptyNd
      badNum2   = SlawNumeric singNf halfNd
      badNum3   = SlawNumeric singNf emptyNd
      badNum4   = SlawNumeric cplxNf halfNd

  True  @=? valSlaw []         halfNum
  False @=? valSlaw [VfCSlaw]  halfNum
  True  @=? valSlaw [VfUtf8]   halfNum
  True  @=? valSlaw [VfDesIng] halfNum

  True  @=? valSlaw []         badUtf8
  True  @=? valSlaw [VfCSlaw]  badUtf8
  False @=? valSlaw [VfUtf8]   badUtf8
  True  @=? valSlaw [VfDesIng] badUtf8

  True  @=? valSlaw []         badDesIng
  False @=? valSlaw [VfCSlaw]  badDesIng
  False @=? valSlaw [VfUtf8]   badDesIng
  False @=? valSlaw [VfDesIng] badDesIng

  True  @=? valSlaw []         goodSym
  False @=? valSlaw [VfCSlaw]  goodSym
  False @=? valSlaw []         badSym1
  False @=? valSlaw []         badSym2

  True  @=? valSlaw []         emptyNum1
  True  @=? valSlaw []         emptyNum2
  False @=? valSlaw []         badNum1
  False @=? valSlaw []         badNum2
  False @=? valSlaw []         badNum3
  False @=? valSlaw []         badNum4

  True  @=? valSlaw []         comprehensiveProtein
  True  @=? valSlaw [VfCSlaw]  comprehensiveProtein
  True  @=? valSlaw [VfUtf8]   comprehensiveProtein
  False @=? valSlaw [VfDesIng] comprehensiveProtein

testSlawMonoid :: Assertion
testSlawMonoid = do
  -- test mconcat with slaw lists
  let lst1 = replicate 3 ","
      lst2 = replicate 2 ","
      lst3 = ["chameleon"] :: [String]
      s1   = š lst1
      s2   = š lst2
      s3   = š lst3
      lst  = concat  [lst1, lst2, lst3]
      slaw = mconcat [s1,   s2,   s3  ]

  Just lst @=? ŝm slaw

  -- test mconcat with slaw strings
  let strs = ["déjà", " ", "vu"] :: [String]
      slx  = map š strs
      str  = concat strs
      slw  = mconcat slx

  š str @=? slw

  -- test mconcat with slaw maps
  let m1  = M.fromList [ ("foo", 1)
                       , ("bar", 2)
                       ]
      m2  = M.fromList [ ("bar", 3)
                       , ("baz", 4)
                       ]
      m3  = M.fromList [ ("foo", 1)
                       , ("bar", 3)
                       , ("baz", 4)
                       ]
      sm1 = š (m1 :: M.Map T.Text Int16)
      sm2 = š (m2 :: M.Map String Int32)
      sm3 = sm1 <> sm2

  Just (m3 :: M.Map T.Text Int) @=? ŝm sm3

  -- test mconcat with numeric slaw
  let a1   = S.fromList [ 1, 2,  3,  4 ]
      a2   = S.fromList [ 5, 6,  7,  8 ]
      a3   = S.fromList [ 9, 10, 11, 12]
      a12  = S.fromList [ 1, 2, 3, 4, 5, 6, 7, 8]
      a13  = a1 <> a3
      nd1  = NumUnt32 a1
      nd2  = NumUnt64 a2
      nd3  = NumUnt32 a3
      nd12 = NumUnt8  a12
      nd13 = NumUnt32 a13
      nf   = NumericFormat { nfArray   = True
                           , nfComplex = False
                           , nfVector  = VtScalar
                           }
      sn1  = SlawNumeric nf nd1
      sn2  = SlawNumeric nf nd2
      sn3  = SlawNumeric nf nd3
      sn12 = SlawNumeric nf nd12
      sn13 = SlawNumeric nf nd13

  sn13        @=? sn1 <> sn3
  sn12        @=? sn1 <> sn2
  sn12 <> sn3 @=? mconcat [sn1, sn2, sn3]

testSlawIO :: Assertion
testSlawIO = do
  let pairs = [ (WriteBinaryOptions pbo af, useName)
              | pbo     <- [minBound..maxBound]
              , af      <- [minBound..maxBound]
              , useName <- [minBound..maxBound]
              ]
  forM_ pairs $ \(wbo, useName) -> do
    let slawx = [mySlaw, š wbo, SlawNil, š (5 :: Int64)]
    roundTripIOwr fpHU slawx wbo useName

  let p   = ("test-files/" ++)
      be2 = p "big-endian-protein-version2.slaw"
      le2 = p "little-endian-protein-version2.slaw"
      kpe = p "kp_enter.slaw"
      ex  = p "example.slaw"
      cle = p "comprehensive-LittleEndian.slaw"
      cbe = p "comprehensive-BigEndian.slaw"
  roundTripIOrw fpHU le2 le2 LittleEndian
  roundTripIOrw fpHU be2 be2 BigEndian
  roundTripIOrw fpHU be2 le2 LittleEndian
  roundTripIOrw fpHU le2 be2 BigEndian
  roundTripIOrw fpHU kpe kpe LittleEndian

  checkSlawRead  ex exampleSlawx
  checkSlawWrite ex exampleSlawx LittleEndian

  let cProt = [comprehensiveProtein]

  checkSlawRead  cle cProt
  checkSlawWrite cle cProt LittleEndian
  checkSlawRead  cbe cProt
  checkSlawWrite cbe cProt BigEndian
