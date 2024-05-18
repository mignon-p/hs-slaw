{-# LANGUAGE ImplicitParams             #-}

module Data.Slaw.Internal.SlawDecode
 ( decodeSlaw
 , decodeProtein
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

decodeSlaw :: ByteOrder -> L.ByteString -> Slaw
decodeSlaw = undefined

decodeProtein :: L.ByteString -> Slaw
decodeProtein = undefined
