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
import Data.List
-- import qualified Data.Vector.Storable    as S
import Data.Word
import GHC.Generics (Generic)
import GHC.Stack
import Text.Printf

import Data.Slaw.Internal.SlawEncode
import Data.Slaw.Internal.SlawType
import Data.Slaw.Internal.Util
-- import Data.Slaw.Internal.VectorConvert

infix 5 //

data Input = Input
  { iLbs ::                 L.ByteString
  , iOff :: {-# UNPACK #-} !Word64       -- byte offset into file/stream
  , iSrc ::                 String       -- name of file/stream
  }

makeInput :: HasCallStack => String -> L.ByteString -> Input
makeInput what lbs = Input { iLbs = lbs
                           , iOff = 0
                           , iSrc = what ++ extra
                           }
  where extra = case getCallStack callStack of
                  (func, loc) : _ ->
                    " passed to " ++ func ++ " at " ++ prettySrcLoc loc
                  _               -> ""

mkErr :: Input -> [String] -> Either String a
mkErr inp ss = Left $ concat $ ss ++ ss'
  where ss' = [ ", at byte offset "
              , show (iOff inp)
              , " of "
              , iSrc inp
              ]

(//) :: Input -> Word64 -> Either String (Input, Input)
inp // nOcts =
  let nBytes       = nOcts * 8
      nBytes'      = fromIntegral nBytes
      (lbs1, lbs2) = L.splitAt nBytes' (iLbs inp)
      lbs1Len      = L.length lbs1
      inp1         = inp { iLbs = lbs1 }
      inp2         = inp { iLbs = lbs2
                         , iOff = iOff inp + nBytes
                         }
  in if lbs1Len /= nBytes'
     then mkErr inp [ "expected "
                    , show nBytes
                    , " bytes but got "
                    , show lbs1Len
                    ]
     else Right (inp1, inp2)

getNib :: Oct -> Nib
getNib o = toEnum $ fromIntegral $ o `shiftR` 60

getNib8 :: Word8 -> Nib
getNib8 o = toEnum $ fromIntegral $ o `shiftR` 4

ethMay :: Maybe a -> Either String a
ethMay Nothing  = Left "decoding error"
ethMay (Just x) = Right x

handleSlawResult :: Either String (Slaw, a) -> Slaw
handleSlawResult (Left  msg   ) = SlawError msg
handleSlawResult (Right (s, _)) = s

decodeSlaw :: HasCallStack => ByteOrder -> L.ByteString -> Slaw
decodeSlaw bo lbs = withFrozenCallStack $
  let ?bo = bo in handleSlawResult $ decodeSlaw1 $ makeInput "slaw" lbs

decodeSlaw1 :: (?bo::ByteOrder) => Input -> Either String (Slaw, Input)
decodeSlaw1 = undefined

decodeProtein :: L.ByteString -> Slaw
decodeProtein lbs = withFrozenCallStack $
  handleSlawResult $ decodeProtein1 $ makeInput "protein" lbs

decodeProtein1 :: Input -> Either String (Slaw, Input)
decodeProtein1 inp = do
  (o, _) <- inp // 1
  byte0  <- ethMay $ iLbs o L.!? 0
  byte7  <- ethMay $ iLbs o L.!? 7
  bo     <- case (getNib8 byte0, getNib8 byte7) of
              (NibSwappedProtein, NibProtein) -> return LittleEndian
              (NibProtein, NibSwappedProtein) -> return BigEndian
              _                               -> proteinErr o
  decodeProtein2 bo inp

decodeProtein2 :: ByteOrder -> Input -> Either String (Slaw, Input)
decodeProtein2 bo inp = let ?bo = bo in decProtein inp

proteinErr :: Input -> Either String a
proteinErr inp =
  let bites  = map (printf "%02X") $ L.unpack (iLbs inp)
      msg    = "does not appear to be a protein"
      msg'   = intercalate " " (bites ++ [msg])
  in mkErr inp [msg']

decProtein :: (?bo::ByteOrder) => Input -> Either String (Slaw, Input)
decProtein = undefined
