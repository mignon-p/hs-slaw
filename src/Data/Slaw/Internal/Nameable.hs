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
-- import Data.Int
import qualified Data.IntMap.Strict       as IM
import Data.List
import Data.Ratio
import qualified Data.Map.Strict          as M
-- import Data.String
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import qualified Data.Vector.Storable     as S
-- import Data.Word
-- import Foreign.Storable
-- import GHC.Generics (Generic)
-- import GHC.Stack
-- import Numeric.Natural
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
    in if tn == "(Integer)"
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
