{-# LANGUAGE ImplicitParams             #-}

module Data.Slaw.Internal.SlawEncoding
  ( ByteOrder(..)     -- re-export
  , nativeByteOrder   -- re-export
  , oppositeByteOrder -- re-export
  , encodeSlaw
  , decodeSlaw
  ) where

import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as R
import qualified Data.ByteString.Lazy    as L
import Data.Int
import qualified Data.Vector.Storable    as S
import Data.Word

import Data.Slaw.Internal.SlawType
import Data.Slaw.Internal.VectorConvert

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
encodeSlaw1 (SlawBool        b           ) = encBool    (fromEnum b)
encodeSlaw1  SlawNil                       = encBool    2
encodeSlaw1 (SlawError       _           ) = encBool    3
encodeSlaw1 (SlawString      lbs         ) = encString  lbs
encodeSlaw1 (SlawList        ss          ) = encList    ss
encodeSlaw1 (SlawMap         pairs       ) = encMap     pairs
encodeSlaw1 (SlawCons        car cdr     ) = encCons    car cdr
encodeSlaw1 (SlawNumeric     nf  nd      ) = enNumeric  nf  nd

decodeSlaw :: ByteOrder -> L.ByteString -> Slaw
decodeSlaw = undefined
