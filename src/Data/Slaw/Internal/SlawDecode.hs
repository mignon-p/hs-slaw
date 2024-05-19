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
infix 5 !?

data Input = Input
  { iLbs ::                 L.ByteString
  , iOff :: {-# UNPACK #-} !Word64       -- byte offset into file/stream
  , iSrc ::                 String       -- name of file/stream
  , iBo  ::                !ByteOrder
  }

makeInput :: HasCallStack => ByteOrder -> String -> L.ByteString -> Input
makeInput bo what lbs = Input { iLbs = lbs
                              , iOff = 0
                              , iSrc = what ++ extra
                              , iBo  = bo
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

(!?) :: Input -> Word64 -> Either String Word8
inp !? idx =
  let lbs  = iLbs inp
      idx' = fromIntegral idx
  in case lbs L.!? idx' of
       Just w8 -> Right w8
       Nothing -> mkErr inp [ "tried to get byte "
                            , show idx
                            , " but only "
                            , show (L.length lbs)
                            , " bytes were present"
                            ]

data NibInfo = NibInfo
  { niName   :: String
  , niLen    :: Oct -> Either String (Word64, Word)
  , niDecode :: Oct -> L.ByteString -> Input -> Either String Slaw
  }

nibInfo :: Nib -> NibInfo
nibInfo NibSwappedProtein = NibInfo "protein"    lenPro' decPro'
nibInfo NibProtein        = NibInfo "protein"    lenPro  decPro
nibInfo NibSymbol         = NibInfo "symbol"     lenSym  decSym
nibInfo NibWeeString      = NibInfo "wee string" lenWstr decWstr
nibInfo NibList           = NibInfo "list"       lenList decList
nibInfo NibMap            = NibInfo "map"        lenMap  decMap
nibInfo NibCons           = NibInfo "cons"       lenCons decCons
nibInfo NibFullString     = NibInfo "string"     lenStr  decStr
nibInfo NibSingleSint     = NibInfo "signed numeric"         lenNum decNum
nibInfo NibSingleUint     = NibInfo "unsigned numeric"       lenNum decNum
nibInfo NibSingleFloat    = NibInfo "floating-point numeric" lenNum decNum
nibInfo NibArraySint      = NibInfo "signed array"           lenNum decNum
nibInfo NibArrayUint      = NibInfo "unsigned array"         lenNum decNum
nibInfo NibArrayFloat     = NibInfo "floating-point array"   lenNum decNum
nibInfo _                 = NibInfo "unknown slaw"           lenUnk decUnk

handleSlawResult :: Either String (Slaw, a) -> Slaw
handleSlawResult (Left  msg   ) = SlawError msg
handleSlawResult (Right (s, _)) = s

decodeSlaw :: HasCallStack => ByteOrder -> L.ByteString -> Slaw
decodeSlaw bo lbs = withFrozenCallStack $
  handleSlawResult $ decodeSlaw1 $ makeInput bo "slaw" lbs

decodeSlaw1 :: Input -> Either String (Slaw, Input)
decodeSlaw1 = undefined

decodeProtein :: L.ByteString -> Slaw
decodeProtein lbs = withFrozenCallStack $
  handleSlawResult $ decodeProtein1 $ makeInput bo "protein" lbs
  where bo = nativeByteOrder

decodeProtein1 :: Input -> Either String (Slaw, Input)
decodeProtein1 inp = do
  byte0  <- inp !? 0
  byte7  <- inp !? 7
  bo     <- case (getNib8 byte0, getNib8 byte7) of
              (NibSwappedProtein, NibProtein) -> return LittleEndian
              (NibProtein, NibSwappedProtein) -> return BigEndian
              _                               -> proteinErr inp
  decodeSlaw1 $ inp { iBo = bo }

proteinErr :: Input -> Either String a
proteinErr inp =
  let bites  = map (printf "%02X") $ L.unpack $ L.take 8 $ iLbs inp
      msg    = "does not appear to be a protein"
      msg'   = intercalate " " (bites ++ [msg])
  in mkErr inp [msg']

lenPro' :: Oct -> Either String (Word64, Word)
lenPro' = undefined

decPro' :: Oct -> L.ByteString -> Input -> Either String Slaw
decPro' = undefined

lenPro :: Oct -> Either String (Word64, Word)
lenPro = undefined

decPro :: Oct -> L.ByteString -> Input -> Either String Slaw
decPro = undefined

lenSym :: Oct -> Either String (Word64, Word)
lenSym = undefined

decSym :: Oct -> L.ByteString -> Input -> Either String Slaw
decSym = undefined

lenWstr :: Oct -> Either String (Word64, Word)
lenWstr = undefined

decWstr :: Oct -> L.ByteString -> Input -> Either String Slaw
decWstr = undefined

lenList :: Oct -> Either String (Word64, Word)
lenList = undefined

decList :: Oct -> L.ByteString -> Input -> Either String Slaw
decList = undefined

lenMap :: Oct -> Either String (Word64, Word)
lenMap = undefined

decMap :: Oct -> L.ByteString -> Input -> Either String Slaw
decMap = undefined

lenCons :: Oct -> Either String (Word64, Word)
lenCons = undefined

decCons :: Oct -> L.ByteString -> Input -> Either String Slaw
decCons = undefined

lenStr :: Oct -> Either String (Word64, Word)
lenStr = undefined

decStr :: Oct -> L.ByteString -> Input -> Either String Slaw
decStr = undefined

lenNum :: Oct -> Either String (Word64, Word)
lenNum = undefined

decNum :: Oct -> L.ByteString -> Input -> Either String Slaw
decNum = undefined

lenUnk :: Oct -> Either String (Word64, Word)
lenUnk = undefined

decUnk :: Oct -> L.ByteString -> Input -> Either String Slaw
decUnk = undefined
