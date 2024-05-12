module Data.Slaw.Internal.SlawImpl
  ( Slaw(..)
  , NumericFormat(..)
  , NumericData(..)
  , VectorType(..)
  , FromSlaw(..)
  , ToSlaw(..)
  , Protein(..)
  , describeSlaw
  ) where

import Control.DeepSeq
import Control.Exception
-- import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import Data.Default.Class
import Data.Hashable
import Data.Int
import Data.List
import qualified Data.Map.Strict      as M
import qualified Data.Text            as T
import qualified Data.Vector.Storable as S
import Data.Word
import Foreign.Storable
import GHC.Generics (Generic)
import GHC.Stack
import System.IO.Unsafe (unsafePerformIO)

import Data.Slaw.Internal.Exception
import Data.Slaw.Internal.Util

data Slaw = SlawProtein (Maybe Slaw) (Maybe Slaw) L.ByteString
          | SlawBool    !Bool
          | SlawNil
          | SlawString  T.Text
          | SlawList    [Slaw]
          | SlawMap     [(Slaw, Slaw)]
          | SlawCons    Slaw Slaw
          | SlawNumeric !NumericFormat NumericData
          | SlawError   String
          deriving (Eq, Ord, Show, Generic, NFData, Hashable)

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
  salt `hashWithSalt` NumInt8   v = salt `hashRawVector` v
  salt `hashWithSalt` NumInt16  v = salt `hashRawVector` v
  salt `hashWithSalt` NumInt32  v = salt `hashRawVector` v
  salt `hashWithSalt` NumInt64  v = salt `hashRawVector` v
  salt `hashWithSalt` NumUnt8   v = salt `hashRawVector` v
  salt `hashWithSalt` NumUnt16  v = salt `hashRawVector` v
  salt `hashWithSalt` NumUnt32  v = salt `hashRawVector` v
  salt `hashWithSalt` NumUnt64  v = salt `hashRawVector` v
  salt `hashWithSalt` NumFloat  v = salt ## S.toList v
  salt `hashWithSalt` NumDouble v = salt ## S.toList v

hashRawVector :: Storable a => Int -> S.Vector a -> Int
hashRawVector salt v
  | len == 0  = salt ## (0xdefacedbadfacade :: Word64)
  | otherwise = unsafePerformIO $ S.unsafeWith v $ \ptr -> do
      let byteLen = len * sizeOf (S.head v)
      hashPtrWithSalt ptr byteLen salt
  where len = S.length v

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

class FromSlaw a where
  fromSlawEither :: Slaw -> Either PlasmaException a

  fromSlawMaybe  :: Slaw -> Maybe a
  fromSlawMaybe s = case fromSlawEither s of
                      Left  _ -> Nothing
                      Right x -> Just x

  fromSlaw       :: HasCallStack => Slaw -> a
  fromSlaw s      = case fromSlawEither s of
                      Left exc ->
                        throw $ exc { peCallstack = Just callStack }
                      Right x  -> x

class ToSlaw a where
  toSlaw :: a -> Slaw

data Protein = Protein
  { pDescrips :: [T.Text]
  , pIngests  :: M.Map T.Text Slaw
  , pRudeData :: L.ByteString
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance Default Protein where
  def = Protein [] M.empty L.empty

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
describeSlaw (SlawProtein _ _ _) = "protein"
describeSlaw (SlawBool    _    ) = "boolean"
describeSlaw (SlawNil          ) = "nil"
describeSlaw (SlawString  _    ) = "string"
describeSlaw (SlawList    _    ) = "list"
describeSlaw (SlawMap     _    ) = "map"
describeSlaw (SlawCons    _ _  ) = "cons"
describeSlaw (SlawNumeric nf nd) = intercalate " " (nfl ++ ndl)
  where nfl =  describeNumericFormat nf
        ndl = [describeNumericData   nd]
describeSlaw (SlawError   _    ) = "corrupt slaw"
