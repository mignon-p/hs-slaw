{-# LANGUAGE ImplicitParams             #-}

module Data.Slaw.Internal.SlawEncode
  ( ByteOrder(..)     -- re-export
  , nativeByteOrder   -- re-export
  , oppositeByteOrder -- re-export
  , encodeSlaw
  , Nib(..)
  , Sym(..)
  , Oct
  , computeBsize
  , vectorSize
  , numericFormatSize
  ) where

import Control.DeepSeq
import Data.Bits
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as R
import qualified Data.ByteString.Lazy    as L
import Data.Hashable
import Data.Int
-- import Data.List
-- import qualified Data.Vector.Storable    as S
import Data.Word
import GHC.Generics (Generic)

import Data.Slaw.Internal.SlawType
import Data.Slaw.Internal.Util
-- import Data.Slaw.Internal.VectorConvert

infixr 9 #>
infixr 9 ##>
infix  5 #!

data Nib =
    NibSwappedProtein   --  0
  | NibProtein          --  1
  | NibSymbol           --  2
  | NibWeeString        --  3
  | NibList             --  4
  | NibMap              --  5
  | NibCons             --  6
  | NibFullString       --  7
  | NibSingleSint       --  8
  | NibSingleUint       --  9
  | NibSingleFloat      -- 10
  | NibReserved11       -- 11
  | NibArraySint        -- 12
  | NibArrayUint        -- 13
  | NibArrayFloat       -- 14
  | NibReserved15       -- 15
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, NFData, Hashable)

data Sym =
    SymFalse -- 0
  | SymTrue  -- 1
  | SymNil   -- 2
  | SymError -- 3
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, NFData, Hashable)

type Oct = Word64

data Octs = Octs
  { oLen :: {-# UNPACK #-} !Word64
  , oBld ::                R.Builder
  } deriving (Show)

instance Semigroup Octs where
  x <> y = Octs (oLen x + oLen y) (oBld x <> oBld y)

instance Monoid Octs where
  mempty = Octs 0 mempty
  mconcat xs = Octs (sum $ map oLen xs) (mconcat $ map oBld xs)

(#>) :: Nib -> Oct -> Oct
nib #> wrd = wrd .|. (nib' `shiftL` 60)
  where nib' = (fromIntegral . fromEnum) nib

(##>) :: Integral a => a -> Oct -> Oct
bite ##> wrd = wrd .|. (fromIntegral bite `shiftL` 56)

encodeSlaw :: ByteOrder -> Slaw -> BinarySlaw
encodeSlaw bo = R.toLazyByteString . encodeSlaw' bo

encodeSlaw' :: ByteOrder -> Slaw -> R.Builder
encodeSlaw' bo s = let ?bo = bo in oBld $ encodeSlaw1 s

encodeSlaw1 :: (?bo::ByteOrder) => Slaw -> Octs
encodeSlaw1 (SlawProtein     des ing rude) = encProtein des ing rude
encodeSlaw1 (SlawBool        b           ) = encSym     b
encodeSlaw1  SlawNil                       = encSym     SymNil
encodeSlaw1 (SlawError       _ _         ) = encSym     SymError
encodeSlaw1 (SlawSymbol      sym         ) = encSymbol  sym
encodeSlaw1 (SlawString      lbs         ) = encString  lbs
encodeSlaw1 (SlawList        ss          ) = encList    NibList ss
encodeSlaw1 (SlawMap         pairs       ) = encMap     pairs
encodeSlaw1 (SlawCons        car cdr     ) = encList    NibCons [car, cdr]
encodeSlaw1 (SlawNumeric     nf  nd      ) = encNumeric nf  nd

encProtein :: (?bo::ByteOrder)
           => Maybe Slaw
           -> Maybe Slaw
           -> L.ByteString
           -> Octs
encProtein des ing rude =
  let (dOcts, dBool)    = encDesIng des
      (iOcts, iBool)    = encDesIng ing
      rudeLen           = L.length rude
      weeRude           = rudeLen <= 7
      top5              = mkTop5 dBool iBool (not weeRude)
      (h2Oct, rudeOcts) = encP2 weeRude (fromIntegral rudeLen) top5 rude
      body              = mconcat [h2Oct, dOcts, iOcts, rudeOcts]
      octLen            = 1 + oLen body
      oLenLo            = octLen .&. 0xf
      oLenHi            = (octLen .&. complement 0xf) `shiftL` 4
      splitLen          = oLenHi .|. oLenLo
      hOct              = encHeader (NibProtein #> splitLen)
  in hOct <> body

encP2 :: (?bo::ByteOrder)
      => Bool
      -> Word64
      -> Word64
      -> L.ByteString
      -> (Octs, Octs)
encP2 True  rudeLen top5 rude =
  (encHeader' (rudeLen ##> top5) (L.toStrict rude), mempty)
encP2 False rudeLen top5 rude =
  (encHeader  (rudeLen .|. top5), (fst . padLbs) rude)

mkTop5 :: Bool -> Bool -> Bool -> Word64
mkTop5 dFlag iFlag bigRude =
  sum [ if dFlag   then bit 62 else 0
      , if iFlag   then bit 61 else 0
      , if bigRude then bit 59 else 0
      ]

encDesIng :: (?bo::ByteOrder) => Maybe Slaw -> (Octs, Bool)
encDesIng Nothing  = (mempty,        False)
encDesIng (Just s) = (encodeSlaw1 s, True)

encSym :: (?bo::ByteOrder, Enum a) => a -> Octs
encSym = encSymbol . fromIntegral . fromEnum

encSymbol :: (?bo::ByteOrder) => Symbol -> Octs
encSymbol sym = encHeader (NibSymbol #> lo56 sym)

encString :: (?bo::ByteOrder) => L.ByteString -> Octs
encString lbs =
  -- Slaw format requires string to include a terminating NUL
  let lbs'   = lbs <> L.singleton 0
      len    = L.length lbs'
  in if len <= 7
     then encHeader' (NibWeeString #> len ##> 0) (L.toStrict lbs')
     else let (body, nPad) = padLbs lbs'
              octLen = 1 + oLen body
              hdr    = encHeader (NibFullString #> nPad ##> octLen)
          in hdr <> body

encList :: (?bo::ByteOrder) => Nib -> [Slaw] -> Octs
encList nib ss = hdr <> body
  where len    = fromIntegral $ length ss
        body   = mconcat $ map encodeSlaw1 ss
        ext    = len >= 15
        len'   = if ext then 15 else len
        octLen = oLen body + if ext then 2 else 1
        hdr1   = encHeader (nib #> len' ##> octLen)
        hdr2   = encHeader len
        hdr    = if ext then hdr1 <> hdr2 else hdr1

encMap :: (?bo::ByteOrder) => [(Slaw, Slaw)] -> Octs
encMap = encList NibMap . map (uncurry SlawCons)

encNumeric :: (?bo::ByteOrder) => NumericFormat -> NumericData -> Octs
encNumeric nf nd =
  let (nt, bs)       = extractNumeric ?bo nd
      (uBits, bsize) = upperBits nf nt
      breadth        = B.length bs `div` bsize
      -- This won't be necessary if numeric data is well-formed,
      -- but just in case it isn't, chop off any extra bytes that
      -- aren't a multiple of bsize.
      bs'            = B.take (bsize * breadth) bs
      (nf', uBits')  = if breadth /= 1 && not (nfArray nf)
                       then let nf2 = nf { nfArray = True }
                                ub2 = fst (upperBits nf2 nt)
                            in (nf2, ub2)
                       else (nf, uBits)
  in encNumeric2 nf' bs' uBits' breadth

encNumeric2 :: (?bo::ByteOrder)
            => NumericFormat
            -> B.ByteString
            -> Int
            -> Int
            -> Octs
encNumeric2 nf bs uBits breadth =
  let isArray  = nfArray nf
      len      = B.length bs
      wee      = not isArray && len <= 4
      hBreadth = if isArray then fromIntegral breadth else 0
      hdr      = (fromIntegral uBits `shiftL` 46) .|. hBreadth
  in if wee
     then encHeader' hdr bs
     else encHeader  hdr <> (fst . padLbs . L.fromStrict) bs

encHeader :: (?bo::ByteOrder) => Oct -> Octs
encHeader !o = Octs 1 bld
  where bld = case ?bo of
                BigEndian    -> R.word64BE o
                LittleEndian -> R.word64LE o

encHeader' :: (?bo::ByteOrder) => Oct -> B.ByteString -> Octs
encHeader' o bs = encHeader (o .|. encSpecial ?bo bs)

encSpecial :: ByteOrder -> B.ByteString -> Oct
encSpecial BigEndian    = encSp1 0 . B.unpack
encSpecial LittleEndian = encSp1 0 . B.unpack . B.reverse

encSp1 :: Oct -> [Word8] -> Oct
encSp1 !o [] = o
encSp1 !o (w8:rest) = encSp1 o' rest
  where o' = fromIntegral w8 .|. (o `shiftL` 8)

padLbs :: L.ByteString -> (Octs, Word64)
padLbs lbs = (body, fromIntegral nPad)
  where (nOcts, nPad) = computePad $ L.length lbs
        padding       = L.replicate nPad 0
        bld           = R.lazyByteString lbs <> R.lazyByteString padding
        body          = Octs (fromIntegral nOcts) bld

-- returns total number of octs, and bytes of padding in last oct
computePad :: Int64 -> (Int64, Int64)
computePad len =
  let (q, r) = len `divMod` 8
  in if r == 0
     then (q, r)
     else (q + 1, 8 - r)

-- returns (top 18 bits of header oct, bsize)
upperBits :: NumericFormat -> NumericType -> (Int, Int)
upperBits nf nt = (uBits, bsize)
  where arrayBit   = (fromEnum . nfArray) nf
        (typ, siz) = classifyNumeric nt
        fuBits     = fromEnum typ
        sizBits    = " 01 2   3" #! siz
        cplxBit    = (fromEnum . nfComplex) nf
        vectBits   = (fromEnum . nfVector ) nf
        bsize      = computeBsize nf siz
        uBits      = sum [ bsize - 1
                         , vectBits `shiftL` 8
                         , cplxBit  `shiftL` 11
                         , sizBits  `shiftL` 12
                         , fuBits   `shiftL` 14
                         , arrayBit `shiftL` 16
                         , 1        `shiftL` 17
                         ]

computeBsize :: NumericFormat -> Int -> Int
computeBsize nf size = size * cplxSize * vectSize
  where cplxSize = if nfComplex nf then 2 else 1
        vectSize = vectorSize (nfVector nf)

vectorSize :: VectorType -> Int
vectorSize vt = "123448@P" #! fromEnum vt

numericFormatSize :: NumericFormat -> Int
numericFormatSize = (`computeBsize` 1)

(#!) :: (Integral a, Integral b) => B.ByteString -> a -> b
bs #! idx = fromIntegral $ (bs `B.index` fromIntegral idx) - 48
