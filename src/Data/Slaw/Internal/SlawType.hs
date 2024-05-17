{-# LANGUAGE PatternSynonyms            #-}

module Data.Slaw.Internal.SlawType
  ( Slaw(.., SlawProtein)
  , NumericFormat(..)
  , NumericData(..)
  , VectorType(..)
  , Symbol
  , describeSlaw
  , removeDups
  ) where

import Control.Arrow (second)
import Control.DeepSeq
-- import Control.Exception
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
-- import Data.Default.Class
import Data.Hashable
import qualified Data.HashMap.Strict     as HM
import Data.Int
import Data.List
-- import qualified Data.Map.Strict      as M
-- import qualified Data.Text            as T
import qualified Data.Vector.Storable as S
import Data.Word
import Foreign.Storable
import GHC.Generics (Generic)
-- import GHC.Stack
import System.IO.Unsafe (unsafePerformIO)

-- import Data.Slaw.Internal.Exception
import Data.Slaw.Internal.Util
import Data.Slaw.Internal.VectorConvert

type Symbol = Word64

data Slaw = SlawProteinRude (Maybe Slaw) (Maybe Slaw) L.ByteString
          | SlawBool        !Bool
          | SlawNil
          | SlawSymbol      !Symbol
          | SlawString      L.ByteString -- UTF-8 encoded
          | SlawList        [Slaw]
          | SlawMap         [(Slaw, Slaw)]
          | SlawCons        Slaw Slaw
          | SlawNumeric     !NumericFormat NumericData
          | SlawError       String
          deriving (Eq, Ord, Show, Generic, NFData, Hashable)

pattern SlawProtein :: Maybe Slaw -> Maybe Slaw -> Slaw
pattern SlawProtein ing des <- SlawProteinRude ing des _ where
  SlawProtein ing des = SlawProteinRude ing des L.empty

{-# COMPLETE SlawProtein, SlawBool, SlawNil, SlawSymbol, SlawString, SlawList, SlawMap, SlawCons, SlawNumeric, SlawError #-}

instance Monoid Slaw where
  mempty = SlawNil
  mconcat = catSlaw . filter (not . isNil)

instance Semigroup Slaw where
  x <> y = mconcat [x, y]

data NumericFormat = NumericFormat
  { nfArray   :: !Bool
  , nfComplex :: !Bool
  , nfVector  :: !VectorType
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)

data NumericData = NumInt8   (S.Vector Int8)
                 | NumInt16  (S.Vector Int16)
                 | NumInt32  (S.Vector Int32)
                 | NumInt64  (S.Vector Int64)
                 | NumUnt8   (S.Vector Word8)
                 | NumUnt16  (S.Vector Word16)
                 | NumUnt32  (S.Vector Word32)
                 | NumUnt64  (S.Vector Word64)
                 | NumFloat  (S.Vector Float)
                 | NumDouble (S.Vector Double)
                 deriving (Eq, Ord, Show, Generic, NFData)

instance Hashable NumericData where
  salt `hashWithSalt` NumInt8   v = hh salt 0 `hashRawVector` v
  salt `hashWithSalt` NumInt16  v = hh salt 1 `hashRawVector` v
  salt `hashWithSalt` NumInt32  v = hh salt 2 `hashRawVector` v
  salt `hashWithSalt` NumInt64  v = hh salt 3 `hashRawVector` v
  salt `hashWithSalt` NumUnt8   v = hh salt 4 `hashRawVector` v
  salt `hashWithSalt` NumUnt16  v = hh salt 5 `hashRawVector` v
  salt `hashWithSalt` NumUnt32  v = hh salt 6 `hashRawVector` v
  salt `hashWithSalt` NumUnt64  v = hh salt 7 `hashRawVector` v
  salt `hashWithSalt` NumFloat  v = hh salt 8 ## S.toList v
  salt `hashWithSalt` NumDouble v = hh salt 9 ## S.toList v

hh :: Int -> Int -> Int
hh salt slug = salt ## (37619* slug)

hashRawVector :: Storable a => Int -> S.Vector a -> Int
hashRawVector salt v
  | len == 0  = salt ## (0xdefacedbadfacade :: Word64)
  | otherwise = unsafePerformIO $ S.unsafeWith v $ \ptr -> do
      let byteLen = len * sizeOf (S.head v)
      hashPtrWithSalt ptr byteLen salt
  where len = S.length v

data NumericType =
    TypInt8
  | TypInt16
  | TypInt32
  | TypInt64
  | TypUnt8
  | TypUnt16
  | TypUnt32
  | TypUnt64
  | TypFloat
  | TypDouble
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, NFData, Hashable)

data VectorType = VtScalar
                | Vt2
                | Vt3
                | Vt4
                | Vt2mv
                | Vt3mv
                | Vt4mv
                | Vt5mv
                deriving (Eq, Ord, Show, Read, Bounded, Enum,
                          Generic, NFData, Hashable)

describeNumericData :: NumericData -> String
describeNumericData (NumInt8   _) =  "8-bit signed integer"
describeNumericData (NumInt16  _) = "16-bit signed integer"
describeNumericData (NumInt32  _) = "32-bit signed integer"
describeNumericData (NumInt64  _) = "64-bit signed integer"
describeNumericData (NumUnt8   _) =  "8-bit unsigned integer"
describeNumericData (NumUnt16  _) = "16-bit unsigned integer"
describeNumericData (NumUnt32  _) = "32-bit unsigned integer"
describeNumericData (NumUnt64  _) = "64-bit unsigned integer"
describeNumericData (NumFloat  _) = "32-bit floating point"
describeNumericData (NumDouble _) = "64-bit floating point"

describeNumericFormat :: NumericFormat -> [String]
describeNumericFormat nf = concat [d1, d2, d3]
  where d1 = if nfArray nf then ["array of"] else []
        d2 = describeVectorType $ nfVector nf
        d3 = if nfComplex nf then ["complex"] else []

describeVectorType :: VectorType -> [String]
describeVectorType VtScalar = []
describeVectorType Vt2   = ["2-vector of"]
describeVectorType Vt3   = ["3-vector of"]
describeVectorType Vt4   = ["4-vector of"]
describeVectorType Vt2mv = ["2-multivector of"]
describeVectorType Vt3mv = ["3-multivector of"]
describeVectorType Vt4mv = ["4-multivector of"]
describeVectorType Vt5mv = ["5-multivector of"]

describeSlaw :: Slaw -> String
describeSlaw (SlawProtein _ _  ) = "protein"
describeSlaw (SlawBool    b    ) = "boolean " ++ show b
describeSlaw (SlawNil          ) = "nil"
describeSlaw (SlawSymbol  s    ) = "symbol " ++ show s
describeSlaw (SlawString  _    ) = "string"
describeSlaw (SlawList    _    ) = "list"
describeSlaw (SlawMap     _    ) = "map"
describeSlaw (SlawCons    _ _  ) = "cons"
describeSlaw (SlawNumeric nf nd) = intercalate " " (nfl ++ ndl)
  where nfl =  describeNumericFormat nf
        ndl = [describeNumericData   nd]
describeSlaw (SlawError   _    ) = "corrupt slaw"

dnf :: NumericFormat -> String
dnf nf = case describeNumericFormat nf of
           [] -> "scalar"
           xs -> let str      = intercalate " " xs
                     len      = length str
                     sfx      = " of"
                     sfxLen   = length sfx
                     (s1, s2) = splitAt (len - sfxLen) str
                 in if sfx == s2 then s1 else str

isNil :: Slaw -> Bool
isNil SlawNil = True
isNil _       = False

typeMismatch :: String
typeMismatch = "type mismatch: "

cantCat :: String -> String -> String
cantCat s1 s2 =
  concat [typeMismatch, "Can't concatenate ", s1, " and ", s2]

catSlaw :: [Slaw] -> Slaw
catSlaw []                        = SlawNil
catSlaw [s]                       = s
catSlaw (s@(SlawError   _)   : _) = s
catSlaw ss@(SlawString  _    : _) = doCat getString       catStrings ss
catSlaw ss@(SlawList    _    : _) = doCat getList         catLists   ss
catSlaw ss@(SlawMap     _    : _) = doCat getMap          catMaps    ss
catSlaw ss@(SlawNumeric nf _ : _) = doCat (getNumeric nf) (catNumeric nf) ss
catSlaw (_ : s@(SlawError _) : _) = s
catSlaw (s1 : s2             : _) =
  SlawError $ describeSlaw s1 `cantCat` describeSlaw s2

doCat :: (Slaw -> Either String a)
      -> ([a] -> Slaw)
      -> [Slaw]
      -> Slaw
doCat chkFunc catFunc ss =
  case mapM chkFunc ss of
    Left msg -> SlawError msg
    Right xs -> catFunc   xs

getString :: Slaw -> Either String L.ByteString
getString (SlawString lbs) = Right lbs
getString (SlawError  msg) = Left msg
getString s                = Left $ "string" `cantCat` describeSlaw s

getList :: Slaw -> Either String [Slaw]
getList (SlawList   ss ) = Right ss
getList (SlawError  msg) = Left msg
getList s                = Left $ "list" `cantCat` describeSlaw s

getMap :: Slaw -> Either String [(Slaw, Slaw)]
getMap (SlawMap   ss ) = Right ss
getMap (SlawError msg) = Left msg
getMap s               = Left $ "map" `cantCat` describeSlaw s

getNumeric :: NumericFormat -> Slaw -> Either String NumericData
getNumeric nf0 (SlawNumeric nf nd) =
  if nfComplex nf0 == nfComplex nf && nfVector nf0 == nfVector nf
  then Right nd
  else Left $ dnf nf0 `cantCat` dnf nf
getNumeric _   (SlawError   msg  ) = Left msg
getNumeric nf0  s                  =
  Left $ dnf nf0 `cantCat` describeSlaw s

catStrings :: [L.ByteString] -> Slaw
catStrings = SlawString . mconcat

catLists :: [[Slaw]] -> Slaw
catLists = SlawList  . concat

catMaps :: [[(Slaw, Slaw)]] -> Slaw
catMaps = SlawMap . removeDups . concat

-- Remove duplicate keys while preserving order.
-- Keeps the *last* value for a key, but at the position
-- where the key *first* appeared.
removeDups :: [(Slaw, Slaw)] -> [(Slaw, Slaw)]
removeDups pairs = map (second snd) l4
  where pairs1      = zipWith f1 pairs [(1 :: Word64)..]
        f1 (k, v) n = (k, (n, v))
        hm          = HM.fromListWith f2 pairs1
        f2 (_, newV) (oldN, _) = (oldN, newV)
        l3          = HM.toList hm
        l4          = sortOn (fst . snd) l3

catNumeric :: NumericFormat -> [NumericData] -> Slaw
catNumeric nf nds = SlawNumeric nf' nd
  where
    nf'         = nf { nfArray = True }
    pairs       = map extractNumeric nds
    (typs, bss) = unzip pairs
    typ0        = head typs
    sameType    = all (== typ0) typs
    nd          = if sameType
                  then restoreNumeric typ0 $ mconcat bss
                  else listToNum $ concatMap numToList nds

extractNumeric :: NumericData -> (NumericType, B.ByteString)
extractNumeric (NumInt8   v) = (TypInt8  , vToBs nativeByteOrder v)
extractNumeric (NumInt16  v) = (TypInt16 , vToBs nativeByteOrder v)
extractNumeric (NumInt32  v) = (TypInt32 , vToBs nativeByteOrder v)
extractNumeric (NumInt64  v) = (TypInt64 , vToBs nativeByteOrder v)
extractNumeric (NumUnt8   v) = (TypUnt8  , vToBs nativeByteOrder v)
extractNumeric (NumUnt16  v) = (TypUnt16 , vToBs nativeByteOrder v)
extractNumeric (NumUnt32  v) = (TypUnt32 , vToBs nativeByteOrder v)
extractNumeric (NumUnt64  v) = (TypUnt64 , vToBs nativeByteOrder v)
extractNumeric (NumFloat  v) = (TypFloat , vToBs nativeByteOrder v)
extractNumeric (NumDouble v) = (TypDouble, vToBs nativeByteOrder v)

restoreNumeric :: NumericType -> B.ByteString -> NumericData
restoreNumeric TypInt8   bs = NumInt8   (bsToV nativeByteOrder bs)
restoreNumeric TypInt16  bs = NumInt16  (bsToV nativeByteOrder bs)
restoreNumeric TypInt32  bs = NumInt32  (bsToV nativeByteOrder bs)
restoreNumeric TypInt64  bs = NumInt64  (bsToV nativeByteOrder bs)
restoreNumeric TypUnt8   bs = NumUnt8   (bsToV nativeByteOrder bs)
restoreNumeric TypUnt16  bs = NumUnt16  (bsToV nativeByteOrder bs)
restoreNumeric TypUnt32  bs = NumUnt32  (bsToV nativeByteOrder bs)
restoreNumeric TypUnt64  bs = NumUnt64  (bsToV nativeByteOrder bs)
restoreNumeric TypFloat  bs = NumFloat  (bsToV nativeByteOrder bs)
restoreNumeric TypDouble bs = NumDouble (bsToV nativeByteOrder bs)

data NumElem = ElemInt    !Integer
             | ElemFloat  !Float
             | ElemDouble !Double
             deriving (Eq, Ord, Show, Generic, NFData, Hashable)

data NumRange = RangeInt  { rangeLo :: !Integer, rangeHi :: !Integer }
              | RangeFloat
              | RangeDouble
              deriving (Eq, Ord, Show, Generic, NFData, Hashable)

numToList :: NumericData -> [NumElem]
numToList (NumInt8   v) = intToList    v
numToList (NumInt16  v) = intToList    v
numToList (NumInt32  v) = intToList    v
numToList (NumInt64  v) = intToList    v
numToList (NumUnt8   v) = intToList    v
numToList (NumUnt16  v) = intToList    v
numToList (NumUnt32  v) = intToList    v
numToList (NumUnt64  v) = intToList    v
numToList (NumFloat  v) = floatToList  v
numToList (NumDouble v) = doubleToList v

intToList :: (Integral a, Storable a) => S.Vector a -> [NumElem]
intToList = map (ElemInt . toInteger) . S.toList

floatToList :: S.Vector Float -> [NumElem]
floatToList = map ElemFloat . S.toList

doubleToList :: S.Vector Double -> [NumElem]
doubleToList = map ElemDouble . S.toList

listToNum :: [NumElem] -> NumericData
listToNum nes =
  let range = foldl' addToRange (baseRange nes) nes
      typ   = typeFromRange range
  in typeToNum typ nes

baseRange :: [NumElem] -> NumRange
baseRange []                 = RangeInt 0 0 -- shouldn't happen
baseRange (ElemInt    x : _) = RangeInt x x
baseRange (ElemFloat  _ : _) = RangeFloat
baseRange (ElemDouble _ : _) = RangeDouble

addToRange :: NumRange -> NumElem -> NumRange
addToRange RangeDouble      _              = RangeDouble
addToRange _                (ElemDouble _) = RangeDouble
addToRange (RangeInt lo hi) (ElemInt x)    = RangeInt lo' hi'
  where lo' = lo `min` x
        hi' = hi `max` x
addToRange RangeFloat       (ElemFloat _)  = RangeFloat
addToRange RangeFloat       (ElemInt x)
  | fitsInFloat x = RangeFloat
  | otherwise     = RangeDouble
addToRange (RangeInt lo hi) (ElemFloat _)
  | fitsInFloat lo && fitsInFloat hi = RangeFloat
  | otherwise                        = RangeDouble

fitsInFloat :: Integer -> Bool
fitsInFloat x = abs x <= 0x1_00_00_00

typeFromRange :: NumRange -> NumericType
typeFromRange RangeFloat  = TypFloat
typeFromRange RangeDouble = TypDouble
typeFromRange (RangeInt lo hi) =
  case find (checkRange lo hi) ranges of
    Nothing       -> TypDouble
    Just (_, typ) -> typ

checkRange :: Integer
           -> Integer
           -> ((Integer, Integer), a)
           -> Bool
checkRange lo hi ((lo', hi'), _) = lo >= lo' && hi <= hi'

ranges :: [((Integer, Integer), NumericType)]
ranges =
  [ mkRange (0 :: Word8 ) TypUnt8
  , mkRange (0 :: Word16) TypUnt16
  , mkRange (0 :: Word32) TypUnt32
  , mkRange (0 :: Word64) TypUnt64
  , mkRange (0 :: Int8  ) TypInt8
  , mkRange (0 :: Int16 ) TypInt16
  , mkRange (0 :: Int32 ) TypInt32
  , mkRange (0 :: Int64 ) TypInt64
  ]

mkRange :: (Integral a, Bounded a)
        => a
        -> NumericType
        -> ((Integer, Integer), NumericType)
mkRange dummy typ = ((toInteger lo, toInteger hi), typ)
  where lo = minBound `asTypeOf` dummy
        hi = maxBound `asTypeOf` dummy

typeToNum :: NumericType -> [NumElem] -> NumericData
typeToNum TypInt8   = NumInt8   . S.fromList . map intCoerce
typeToNum TypInt16  = NumInt16  . S.fromList . map intCoerce
typeToNum TypInt32  = NumInt32  . S.fromList . map intCoerce
typeToNum TypInt64  = NumInt64  . S.fromList . map intCoerce
typeToNum TypUnt8   = NumUnt8   . S.fromList . map intCoerce
typeToNum TypUnt16  = NumUnt16  . S.fromList . map intCoerce
typeToNum TypUnt32  = NumUnt32  . S.fromList . map intCoerce
typeToNum TypUnt64  = NumUnt64  . S.fromList . map intCoerce
typeToNum TypFloat  = NumFloat  . S.fromList . map floatCoerce
typeToNum TypDouble = NumDouble . S.fromList . map doubleCoerce

intCoerce :: (Integral a, Storable a) => NumElem -> a
intCoerce (ElemInt    x) = fromInteger x
intCoerce (ElemFloat  x) = round x -- shouldn't happen
intCoerce (ElemDouble x) = round x -- shouldn't happen

floatCoerce :: NumElem -> Float
floatCoerce (ElemInt    x) = fromInteger  x
floatCoerce (ElemFloat  x) = x
floatCoerce (ElemDouble x) = double2Float x -- shouldn't happen

doubleCoerce :: NumElem -> Double
doubleCoerce (ElemInt    x) = fromInteger  x
doubleCoerce (ElemFloat  x) = float2Double x
doubleCoerce (ElemDouble x) = x
