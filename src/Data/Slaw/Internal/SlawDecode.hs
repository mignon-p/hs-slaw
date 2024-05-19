module Data.Slaw.Internal.SlawDecode
 ( decodeSlaw
 , decodeProtein
 ) where

import Control.DeepSeq
import Control.Monad
import Data.Bits
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as R
import qualified Data.ByteString.Lazy    as L
import Data.Char
import Data.Hashable
import Data.Int
import Data.List
import qualified Data.Map.Strict         as M
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

type Special = B.ByteString

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
mkErr inp ss = Left $ fmtErr inp ss

fmtErr :: Input -> [String] -> String
fmtErr inp ss = concat $ ss ++ ss'
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
  , niDecode :: Oct -> Special -> Input -> Either String Slaw
  }

nibInfo :: Nib -> NibInfo
nibInfo NibSwappedProtein = NibInfo "protein"    lenPro'      decPro'
nibInfo NibProtein        = NibInfo "protein"    lenPro       decPro
nibInfo NibSymbol         = NibInfo "symbol"     lenSym       decSym
nibInfo NibWeeString      = NibInfo "wee string" lenWstr      decWstr
nibInfo NibList           = NibInfo "list"       lenContainer decList
nibInfo NibMap            = NibInfo "map"        lenContainer decMap
nibInfo NibCons           = NibInfo "cons"       lenCons      decCons
nibInfo NibFullString     = NibInfo "string"     lenStr       decStr
nibInfo NibSingleSint  = NibInfo "signed numeric"
                         (lenNum False) (decNum (False, NumTypSigned))
nibInfo NibSingleUint  = NibInfo "unsigned numeric"
                         (lenNum False) (decNum (False, NumTypUnsigned))
nibInfo NibSingleFloat = NibInfo "floating-point numeric"
                         (lenNum False) (decNum (False, NumTypFloat))
nibInfo NibArraySint   = NibInfo "signed array"
                         (lenNum True)  (decNum (True,  NumTypSigned))
nibInfo NibArrayUint   = NibInfo "unsigned array"
                         (lenNum True)  (decNum (True,  NumTypUnsigned))
nibInfo NibArrayFloat  = NibInfo "floating-point array"
                         (lenNum True)  (decNum (True,  NumTypFloat))
nibInfo _              = NibInfo "unknown slaw"  lenUnk  decUnk

handleSlawResult :: Either String (Slaw, a) -> Slaw
handleSlawResult (Left  msg   ) = SlawError msg
handleSlawResult (Right (s, _)) = s

decodeSlaw :: HasCallStack => ByteOrder -> L.ByteString -> Slaw
decodeSlaw bo lbs = withFrozenCallStack $
  handleSlawResult $ decodeSlaw1 $ makeInput bo "slaw" lbs

decodeSlaw1 :: Input -> Either String (Slaw, Input)
decodeSlaw1 inp = do
  (hdr, rest) <- inp // 1
  let hdrBs = (L.toStrict . iLbs) hdr
      bo    = iBo hdr
      oHdr  = decodeOct bo hdrBs
      nib   = getNib oHdr
      info  = nibInfo nib
      ctx   = [ "in "
              , niName info
              , " (header oct "
              , showOct oHdr
              , "), "
              ]
  (octLen, nSpecial) <- withMore (addCtx hdr ctx) $ niLen info oHdr
  let special = getSpecial bo hdrBs nSpecial
  when (octLen == 0) $ mkErr inp $ ctx ++ ["octlen of 0 is not allowed"]
  (body, leftover) <- withMore (addCtx rest ctx) $ rest // (octLen - 1)
  case niDecode info oHdr special body of
    Left msg -> Right (SlawError $ concat $ ctx ++ [msg], leftover)
    Right s  -> Right (s, leftover)

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
lenPro' = lenPro . byteSwap64

decPro' :: Oct -> Special -> Input -> Either String Slaw
decPro' o _ inp = decPro (byteSwap64 o) mempty inp'
  where inp' = inp { iBo = oppositeByteOrder (iBo inp) }

lenPro :: Oct -> Either String (Word64, Word)
lenPro = undefined

decPro :: Oct -> Special -> Input -> Either String Slaw
decPro = undefined

lenSym :: Oct -> Either String (Word64, Word)
lenSym _ = Right (1, 0)

decSym :: Oct -> Special -> Input -> Either String Slaw
decSym o _ _ = (Right . symbol2slaw . lo56) o

symbol2slaw :: Symbol -> Slaw
symbol2slaw s
  | s <= maxSymbol = sym2slaw $ toEnum $ fromIntegral s
  | otherwise      = SlawSymbol s
  where maxSym    = maxBound :: Sym
        maxSymbol = (fromIntegral . fromEnum) maxSym

sym2slaw :: Sym -> Slaw
sym2slaw SymFalse = SlawBool False
sym2slaw SymTrue  = SlawBool True
sym2slaw SymNil   = SlawNil
sym2slaw SymError = SlawError msg
  where msg = "(The result of round-tripping a previously detected error)"

lenWstr :: Oct -> Either String (Word64, Word)
lenWstr o = do
  checkBits stringReservedBit o
  return (1, penultimateNibble o)

decWstr :: Oct -> Special -> Input -> Either String Slaw
decWstr _ spec _ = (Right . SlawString . trimNul . L.fromStrict) spec

decList :: Oct -> Special -> Input -> Either String Slaw
decList o _ inp = do
  let nElems0 = penultimateNibble o
  (nElems, inp') <- if nElems0 < 15
                    then return (nElems0, inp)
                    else longerLen inp
  return $ SlawList $ decodeSequence nElems 0 inp'

decodeSequence :: Word64 -> Word64 -> Input -> [Slaw]
decodeSequence !nElems !idx inp =
  let exhausted = (L.null . iLbs) inp
      finished  = idx >= nElems
      slawE xs  = [ (SlawError . fmtErr inp) xs ]
  in case (exhausted, finished) of
       (True, True)  -> []
       (False, True) -> slawE [ "There should have been "
                              , show nElems
                              , " elements, but there "
                              , "are more than that"
                              ]
       (True, False) -> slawE [ "Expected "
                              , show nElems
                              , " elements, but only got "
                              , show idx
                              ]
       (False, False) ->
         case decodeSlaw1 inp of
           Left msg        -> [SlawError msg]
           Right (s, inp') -> s : decodeSequence nElems (idx + 1) inp'

longerLen :: Input -> Either String (Word64, Input)
longerLen inp = do
  (inp1, inp2) <- inp // 1
  let bs = (L.toStrict . iLbs) inp1
  return (decodeOct (iBo inp1) bs, inp2)

decMap :: Oct -> Special -> Input -> Either String Slaw
decMap = undefined

lenCons :: Oct -> Either String (Word64, Word)
lenCons o =
  let nElems = penultimateNibble o
  in if nElems /= (2 :: Int)
     then Left $ concat [ "Cons should have 2 elements, "
                        , "but claims to have "
                        , show nElems
                        , " elements"
                        ]
     else lenContainer o

decCons :: Oct -> Special -> Input -> Either String Slaw
decCons = undefined

lenContainer :: Oct -> Either String (Word64, Word)
lenContainer o = Right (lo56 o, 0)

lenStr :: Oct -> Either String (Word64, Word)
lenStr o = do
  checkBits stringReservedBit o
  return (lo56 o, 0)

decStr :: Oct -> Special -> Input -> Either String Slaw
decStr o _ inp = (Right . SlawString . trimNul) lbs
  where padding = penultimateNibble o
        lbs     = L.dropEnd padding (iLbs inp)

lenNum :: Bool -> Oct -> Either String (Word64, Word)
lenNum = undefined

decNum :: (Bool, NumTyp)
       -> Oct
       -> Special
       -> Input
       -> Either String Slaw
decNum = undefined

lenUnk :: Oct -> Either String (Word64, Word)
lenUnk = Left . unkMsg

decUnk :: Oct -> Special -> Input -> Either String Slaw
decUnk o _ inp = mkErr inp [unkMsg o]

unkMsg :: Oct -> String
unkMsg o = printf "Most-significant nibble is reserved value 0x%x" nib
  where nib = o `shiftR` 60

lo56 :: Oct -> Word64
lo56 = (.&. complement 0xff00_0000_0000_0000)

stringReservedBit :: [(Int, String)]
stringReservedBit = [(59, "reserved")]

penultimateNibble :: Integral a => Oct -> a
penultimateNibble o = fromIntegral $ 0xf .&. (o `shiftR` 56)

-- If the string ends in a NUL byte (which it should), remove it.
trimNul :: L.ByteString -> L.ByteString
trimNul lbs =
  case L.unsnoc lbs of
    Just (lbs', 0) -> lbs'
    _              -> lbs

checkBits :: [(Int, String)] -> Oct -> Either String ()
checkBits pairs o =
  let badBits    = filter f pairs
      f (pos, _) = o `testBit` pos
  in case badBits of
       []    -> Right ()
       [_]   -> Left $ bitMsg "bit"  "was"  " "  badBits
       [_,_] -> Left $ bitMsg "bits" "were" " "  badBits
       _     -> Left $ bitMsg "bits" "were" ", " badBits

bitMsg :: String -> String -> String -> [(Int, String)] -> String
bitMsg noun verb sep pairs = (ucFirst . concat . reverse) revStrs
  where
    revPairs = reverse pairs
    bm0      = bitMsg0 noun
    sfx      = " " ++ verb ++ " unexpectedly set"
    revStrs  = zipWith bm0 revPairs [sfx, sep ++ "and ", cycle sep]

bitMsg0 :: String -> (Int, String) -> String -> String
bitMsg0 noun (pos, desc) sep = concat [ noun
                                      , " "
                                      , show pos
                                      , optDesc desc
                                      , sep
                                      ]
  where optDesc "" = ""
        optDesc x  = " (" ++ x ++ ")"

numMap :: M.Map (NumTyp, Int) NumericType
numMap = M.fromList $ map f [minBound..maxBound]
  where f nt = (classifyNumeric nt, nt)

unclassifyNumeric :: (NumTyp, Int) -> Either [String] NumericType
unclassifyNumeric pair@(t, nb) =
  case pair `M.lookup` numMap of
    Just nt -> Right nt
    Nothing -> Left msg
  where msg = [ show (8 * nb)
              , "-bit "
              , (map toLower . drop 6 . show) t
              , " not supported"
              ]

decodeOct :: ByteOrder -> B.ByteString -> Oct
decodeOct bo lbs = sum xs
  where xs      = zipWith f (B.unpack lbs) (bitPositions bo)
        f x pos = fromIntegral x `shiftL` pos

bitPositions :: ByteOrder -> [Int]
bitPositions LittleEndian = [0,8..56]
bitPositions BigEndian    = [56,48..0]

getSpecial :: ByteOrder -> B.ByteString -> Word -> Special
getSpecial _            _   0 = B.empty
getSpecial LittleEndian lbs n = B.take (fromIntegral n) lbs
getSpecial BigEndian    lbs n = B.take (fromIntegral n) lbs'
  where lbs' = B.drop (8 - fromIntegral n) lbs

showOct :: Oct -> String
showOct o = printf "%04X_%04X_%04X_%04X" (f 3) (f 2) (f 1) (f 0)
  where f = get16 o

get16 :: Oct -> Int -> Word16
get16 o n = fromIntegral $ o `shiftR` (n * 16)

withMore :: (String -> String) -> Either String a -> Either String a
withMore f (Left msg)  = Left (f msg)
withMore _ x@(Right _) = x

addCtx :: Input -> [String] -> String -> String
addCtx inp ss s = fmtErr inp (ss ++ [s])

addCtx' :: [String] -> String -> String
addCtx' ss s = concat (ss ++ [s])
