{-# LANGUAGE ScopedTypeVariables        #-}

module Data.Slaw.Internal.Nameable
  ( Nameable(..)
  ) where

-- import Control.DeepSeq
-- import Control.Exception
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as L
import qualified Data.ByteString.Short    as SBS
-- import Data.Char
-- import Data.Default.Class
-- import Data.Either
-- import Data.Hashable
import qualified Data.HashMap.Strict      as HM
import Data.Int
import qualified Data.IntMap.Strict       as IM
import Data.List
import Data.Ratio
import qualified Data.Map.Strict          as M
-- import Data.String
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import qualified Data.Vector.Storable     as S
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
-- import GHC.Generics (Generic)
-- import GHC.Stack
import Numeric.Natural
-- import System.IO.Unsafe (unsafePerformIO)
-- import Text.Read

import Data.Slaw.Internal.NumericTypes
import Data.Slaw.Internal.SlawType

class Nameable a where
  typeName :: a -> String

typeName' :: Nameable a => a -> String
typeName' x =
  let tn = typeName x
  in if ' ' `elem` tn
     then '(' : tn ++ ")"
     else tn

mkTupleName :: [String] -> String
mkTupleName names = "(" ++ intercalate ", " names ++ ")"

instance Nameable Slaw where
  typeName _ = "Slaw"

instance Nameable a => Nameable [a] where
  typeName _ =
    let tn = typeName (undefined :: a)
    in if tn == "Char"
       then "String"
       else "[" ++ tn ++ "]"

instance Nameable Bool where
  typeName _ = "Bool"

instance Nameable () where
  typeName _ = "()"

instance Nameable T.Text where
  typeName _ = "Text"

instance Nameable LT.Text where
  typeName _ = "lazy Text"

instance Nameable Char where
  typeName _ = "Char"

instance Nameable Integer where
  typeName _ = "Integer"

instance Nameable B.ByteString where
  typeName _ = "ByteString"

instance Nameable L.ByteString where
  typeName _ = "lazy ByteString"

instance Nameable SBS.ShortByteString where
  typeName _ = "ShortByteString"

instance (Nameable a, Nameable b) => Nameable (a, b) where
  typeName _ = mkTupleName [ typeName (undefined :: a)
                           , typeName (undefined :: b)
                           ]

instance ( Nameable a
         , Nameable b
         , Nameable c
         ) => Nameable (a, b, c) where
  typeName _ = mkTupleName [ typeName (undefined :: a)
                           , typeName (undefined :: b)
                           , typeName (undefined :: c)
                           ]

instance ( Nameable a
         , Nameable b
         , Nameable c
         , Nameable d
         ) => Nameable (a, b, c, d) where
  typeName _ = mkTupleName [ typeName (undefined :: a)
                           , typeName (undefined :: b)
                           , typeName (undefined :: c)
                           , typeName (undefined :: d)
                           ]

instance ( Nameable a
         , Nameable b
         , Nameable c
         , Nameable d
         , Nameable e
         ) => Nameable (a, b, c, d, e) where
  typeName _ = mkTupleName [ typeName (undefined :: a)
                           , typeName (undefined :: b)
                           , typeName (undefined :: c)
                           , typeName (undefined :: d)
                           , typeName (undefined :: e)
                           ]

instance ( Nameable a
         , Nameable b
         , Nameable c
         , Nameable d
         , Nameable e
         , Nameable f
         ) => Nameable (a, b, c, d, e, f) where
  typeName _ = mkTupleName [ typeName (undefined :: a)
                           , typeName (undefined :: b)
                           , typeName (undefined :: c)
                           , typeName (undefined :: d)
                           , typeName (undefined :: e)
                           , typeName (undefined :: f)
                           ]

instance ( Nameable a
         , Nameable b
         , Nameable c
         , Nameable d
         , Nameable e
         , Nameable f
         , Nameable g
         ) => Nameable (a, b, c, d, e, f, g) where
  typeName _ = mkTupleName [ typeName (undefined :: a)
                           , typeName (undefined :: b)
                           , typeName (undefined :: c)
                           , typeName (undefined :: d)
                           , typeName (undefined :: e)
                           , typeName (undefined :: f)
                           , typeName (undefined :: g)
                           ]

instance ( Nameable a
         , Nameable b
         , Nameable c
         , Nameable d
         , Nameable e
         , Nameable f
         , Nameable g
         , Nameable h
         ) => Nameable (a, b, c, d, e, f, g, h) where
  typeName _ = mkTupleName [ typeName (undefined :: a)
                           , typeName (undefined :: b)
                           , typeName (undefined :: c)
                           , typeName (undefined :: d)
                           , typeName (undefined :: e)
                           , typeName (undefined :: f)
                           , typeName (undefined :: g)
                           , typeName (undefined :: h)
                           ]

instance (Nameable a, Nameable b) => Nameable (M.Map a b) where
  typeName _ = concat [ "Map "
                      , typeName' (undefined :: a)
                      , " "
                      , typeName' (undefined :: b)
                      ]

instance (Nameable a, Nameable b) => Nameable (HM.HashMap a b) where
  typeName _ = concat [ "HashMap "
                      , typeName' (undefined :: a)
                      , " "
                      , typeName' (undefined :: b)
                      ]

instance (Nameable b) => Nameable (IM.IntMap b) where
  typeName _ = concat [ "IntMap "
                      , typeName' (undefined :: b)
                      ]

instance (Nameable a, Nameable b) => Nameable (Either a b) where
  typeName _ = concat [ "Either "
                      , typeName' (undefined :: a)
                      , " "
                      , typeName' (undefined :: b)
                      ]

instance Nameable a => Nameable (Maybe a) where
  typeName _ = "Maybe " ++ typeName' (undefined :: a)

instance Nameable a => Nameable (Ratio a) where
  typeName _ =
    let tn = typeName' (undefined :: a)
    in if tn == "Integer"
       then "Rational"
       else "Ratio " ++ tn

instance Nameable a => Nameable (V2 a) where
  typeName _ = "V2 " ++ typeName' (undefined :: a)

instance Nameable a => Nameable (V3 a) where
  typeName _ = "V3 " ++ typeName' (undefined :: a)

instance Nameable a => Nameable (V4 a) where
  typeName _ = "V4 " ++ typeName' (undefined :: a)

instance Nameable a => Nameable (S.Vector a) where
  typeName _ = "Vector " ++ typeName' (undefined :: a)

instance Nameable Int8 where
  typeName _ = "Int8"

instance Nameable Int16 where
  typeName _ = "Int16"

instance Nameable Int32 where
  typeName _ = "Int32"

instance Nameable Int64 where
  typeName _ = "Int64"

instance Nameable Word8 where
  typeName _ = "Word8"

instance Nameable Word16 where
  typeName _ = "Word16"

instance Nameable Word32 where
  typeName _ = "Word32"

instance Nameable Word64 where
  typeName _ = "Word64"

instance Nameable Int where
  typeName _ = "Int"

instance Nameable Word where
  typeName _ = "Word"

instance Nameable Float where
  typeName _ = "Float"

instance Nameable Double where
  typeName _ = "Double"

instance Nameable Natural where
  typeName _ = "Natural"

instance Nameable CBool where
  typeName _ = "CBool"

instance Nameable CChar where
  typeName _ = "CChar"

instance Nameable CInt where
  typeName _ = "CInt"

instance Nameable CIntMax where
  typeName _ = "CIntMax"

instance Nameable CIntPtr where
  typeName _ = "CIntPtr"

instance Nameable CLLong where
  typeName _ = "CLLong"

instance Nameable CLong where
  typeName _ = "CLong"

instance Nameable CPtrdiff where
  typeName _ = "CPtrdiff"

instance Nameable CSChar where
  typeName _ = "CSChar"

instance Nameable CShort where
  typeName _ = "CShort"

instance Nameable CSigAtomic where
  typeName _ = "CSigAtomic"

instance Nameable CSize where
  typeName _ = "CSize"

instance Nameable CUChar where
  typeName _ = "CUChar"

instance Nameable CUInt where
  typeName _ = "CUInt"

instance Nameable CUIntMax where
  typeName _ = "CUIntMax"

instance Nameable CUIntPtr where
  typeName _ = "CUIntPtr"

instance Nameable CULLong where
  typeName _ = "CULLong"

instance Nameable CULong where
  typeName _ = "CULong"

instance Nameable CUShort where
  typeName _ = "CUShort"

instance Nameable CWchar where
  typeName _ = "CWchar"

instance Nameable IntPtr where
  typeName _ = "IntPtr"

instance Nameable WordPtr where
  typeName _ = "WordPtr"
