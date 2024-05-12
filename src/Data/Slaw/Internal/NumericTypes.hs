module Data.Slaw.Internal.NumericTypes
  ( V2(..)
  , V3(..)
  , V4(..)
  ) where

import Control.DeepSeq
-- import Control.Exception
-- import qualified Data.ByteString      as B
-- import qualified Data.ByteString.Lazy as L
-- import Data.Complex
import Data.Default.Class
import Data.Hashable
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics (Generic)
-- import GHC.Stack
-- import System.IO.Unsafe (unsafePerformIO)

-- import Data.Slaw.Internal.SlawType
-- import Data.Slaw.Internal.Util

data V2 a = V2 { v2x :: !a
               , v2y :: !a
               } deriving (Eq, Ord, Show, Functor,
                           Generic, NFData, Hashable)

instance Default a => Default (V2 a) where
  def = V2 def def

instance Storable a => Storable (V2 a) where
  sizeOf    v = sizeOf    (v2x v) * 2
  alignment v = alignment (v2x v)

  peek ptr = do
    let ptr' = castPtr ptr
    x <- peekElemOff ptr' 0
    y <- peekElemOff ptr' 1
    return $ V2 x y

  poke ptr v = do
    let ptr' = castPtr ptr
    pokeElemOff ptr' 0 (v2x v)
    pokeElemOff ptr' 1 (v2y v)

data V3 a = V3 { v3x :: !a
               , v3y :: !a
               , v3z :: !a
               } deriving (Eq, Ord, Show, Functor,
                           Generic, NFData, Hashable)

instance Default a => Default (V3 a) where
  def = V3 def def def

instance Storable a => Storable (V3 a) where
  sizeOf    v = sizeOf    (v3x v) * 3
  alignment v = alignment (v3x v)

  peek ptr = do
    let ptr' = castPtr ptr
    x <- peekElemOff ptr' 0
    y <- peekElemOff ptr' 1
    z <- peekElemOff ptr' 2
    return $ V3 x y z

  poke ptr v = do
    let ptr' = castPtr ptr
    pokeElemOff ptr' 0 (v3x v)
    pokeElemOff ptr' 1 (v3y v)
    pokeElemOff ptr' 2 (v3z v)

data V4 a = V4 { v4x :: !a
               , v4y :: !a
               , v4z :: !a
               , v4w :: !a
               } deriving (Eq, Ord, Show, Functor,
                           Generic, NFData, Hashable)

instance Default a => Default (V4 a) where
  def = V4 def def def def

instance Storable a => Storable (V4 a) where
  sizeOf    v = sizeOf    (v4x v) * 4
  alignment v = alignment (v4x v)

  peek ptr = do
    let ptr' = castPtr ptr
    x <- peekElemOff ptr' 0
    y <- peekElemOff ptr' 1
    z <- peekElemOff ptr' 2
    w <- peekElemOff ptr' 3
    return $ V4 x y z w

  poke ptr v = do
    let ptr' = castPtr ptr
    pokeElemOff ptr' 0 (v4x v)
    pokeElemOff ptr' 1 (v4y v)
    pokeElemOff ptr' 2 (v4z v)
    pokeElemOff ptr' 3 (v4w v)
