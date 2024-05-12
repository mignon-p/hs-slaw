module Data.Slaw.Internal.SlawConvert
  ( FromSlaw(..)
  , ToSlaw(..)
  , š
  , ŝ
  , ŝm
  , ŝes
  , ŝee
  , (?:)
  , Protein(..)
  ) where

import Control.DeepSeq
import Control.Exception
-- import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import Data.Default.Class
import Data.Hashable
--- import Data.Int
-- import Data.List
import qualified Data.Map.Strict      as M
import qualified Data.Text            as T
-- import qualified Data.Vector.Storable as S
-- import Data.Word
-- import Foreign.Storable
import GHC.Generics (Generic)
import GHC.Stack
-- import System.IO.Unsafe (unsafePerformIO)

import Data.Slaw.Internal.Exception
import Data.Slaw.Internal.SlawType
-- import Data.Slaw.Internal.Util

---- FromSlaw and ToSlaw classes

class FromSlaw a where
  fromSlaw :: Slaw -> Either PlasmaException a

class ToSlaw a where
  toSlaw :: a -> Slaw

---- shorthand functions for converting to/from slaw

-- "to slaw" because arrow points towards "s"
{-# INLINE š #-}
š :: ToSlaw a => a -> Slaw
š = toSlaw

-- "from slaw" because arrow points away from "s"
{-# INLINABLE ŝ #-}
ŝ :: (HasCallStack, FromSlaw a) => Slaw -> a
ŝ s = case fromSlaw s of
        Left exc ->
          throw $ exc { peCallstack = Just callStack }
        Right x  -> x

-- "from slaw, maybe"
{-# INLINABLE ŝm #-}
ŝm :: FromSlaw a => Slaw -> Maybe a
ŝm s = case fromSlaw s of
         Left  _ -> Nothing
         Right x -> Just x

-- "from slaw, either string"
{-# INLINABLE ŝes #-}
ŝes :: FromSlaw a => Slaw -> Either String a
ŝes s = case fromSlaw s of
          Left exc -> Left $ displayException exc
          Right x  -> Right x

-- "from slaw, either exception"
{-# INLINE ŝee #-}
ŝee :: FromSlaw a => Slaw -> Either PlasmaException a
ŝee = fromSlaw

-- from slaw, with default value
{-# INLINABLE (?:) #-}
(?:) :: FromSlaw a => Slaw -> a -> a
s ?: dflt = case fromSlaw s of
              Left  _ -> dflt
              Right x -> x

---- types

data Protein = Protein
  { pDescrips :: [T.Text]
  , pIngests  :: M.Map T.Text Slaw
  , pRudeData :: L.ByteString
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance Default Protein where
  def = Protein [] M.empty L.empty
