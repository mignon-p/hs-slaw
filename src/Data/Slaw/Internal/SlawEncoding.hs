{-# LANGUAGE ImplicitParams             #-}

module Data.Slaw.Internal.SlawEncoding
  ( ByteOrder(..)     -- re-export
  , nativeByteOrder   -- re-export
  , oppositeByteOrder -- re-export
  , encodeSlaw
  , decodeSlaw
  ) where

import Control.DeepSeq
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as R
import qualified Data.ByteString.Lazy    as L
import Data.Hashable
import Data.Int
import qualified Data.Vector.Storable    as S
import Data.Word
import GHC.Generics (Generic)

import Data.Slaw.Internal.SlawType
import Data.Slaw.Internal.VectorConvert

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

data Octs = Octs
  { oLen :: {-# UNPACK #-} !Int
  , oBld ::                R.Builder
  } deriving (Show)

instance Semigroup Octs where
  x <> y = Octs (oLen x + oLen y) (oBld x <> oBld y)

instance Monoid Octs where
  mempty = Octs 0 mempty
  mconcat xs = Octs (sum $ map oLen xs) (mconcat $ map oBld xs)

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
encodeSlaw1 (SlawList        ss          ) = encList    ss
encodeSlaw1 (SlawMap         pairs       ) = encMap     pairs
encodeSlaw1 (SlawCons        car cdr     ) = encCons    car cdr
encodeSlaw1 (SlawNumeric     nf  nd      ) = enNumeric  nf  nd

encSym :: (?bo::ByteOrder, Enum a) => a -> Octs
encSym = encSymbol . fromIntegral . fromEnum

encSymbol :: (?bo::ByteOrder) => Symbol -> Octs
encSymbol = undefined

decodeSlaw :: ByteOrder -> L.ByteString -> Slaw
decodeSlaw = undefined
