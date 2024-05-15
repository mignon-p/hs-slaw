{-# LANGUAGE ImplicitParams             #-}

module Data.Slaw.Internal.SlawEncoding
  ( ByteOrder(..)     -- re-export
  , nativeByteOrder   -- re-export
  , oppositeByteOrder -- re-export
  , encodeSlaw
  , decodeSlaw
  ) where

import Control.DeepSeq
import Data.Bits
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as R
import qualified Data.ByteString.Lazy    as L
import Data.Hashable
-- import Data.Int
-- import qualified Data.Vector.Storable    as S
import Data.Word
import GHC.Generics (Generic)

import Data.Slaw.Internal.SlawType
import Data.Slaw.Internal.VectorConvert

infixr 9 #>
infixr 9 ##>

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
  { oLen :: {-# UNPACK #-} !Int
  , oBld ::                R.Builder
  } deriving (Show)

instance Semigroup Octs where
  x <> y = Octs (oLen x + oLen y) (oBld x <> oBld y)

instance Monoid Octs where
  mempty = Octs 0 mempty
  mconcat xs = Octs (sum $ map oLen xs) (mconcat $ map oBld xs)

(#>) :: Nib -> Oct -> Oct
nib #> wrd = wrd .|. (nib' `shiftL` 56)
  where nib' = (fromIntegral . fromEnum) nib

(##>) :: Word8 -> Oct -> Oct
nib ##> wrd = wrd .|. (fromIntegral nib `shiftL` 56)

encodeSlaw :: ByteOrder -> Slaw -> L.ByteString
encodeSlaw bo = R.toLazyByteString . encodeSlaw' bo

encodeSlaw' :: ByteOrder -> Slaw -> R.Builder
encodeSlaw' bo s = let ?bo = bo in oBld $ encodeSlaw1 s

encodeSlaw1 :: (?bo::ByteOrder) => Slaw -> Octs
encodeSlaw1 (SlawProteinRude ing des rude) = encProtein ing des rude
encodeSlaw1 (SlawBool        b           ) = encSym     b
encodeSlaw1  SlawNil                       = encSym     SymNil
encodeSlaw1 (SlawError       _           ) = encSym     SymError
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
encProtein = undefined

encSym :: (?bo::ByteOrder, Enum a) => a -> Octs
encSym = encSymbol . fromIntegral . fromEnum

encSymbol :: (?bo::ByteOrder) => Symbol -> Octs
encSymbol = undefined

encString :: (?bo::ByteOrder) => L.ByteString -> Octs
encString = undefined

encList :: (?bo::ByteOrder) => Nib -> [Slaw] -> Octs
encList = undefined

encMap :: (?bo::ByteOrder) => [(Slaw, Slaw)] -> Octs
encMap = undefined

encNumeric :: (?bo::ByteOrder) => NumericFormat -> NumericData -> Octs
encNumeric = undefined

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

decodeSlaw :: ByteOrder -> L.ByteString -> Slaw
decodeSlaw = undefined
