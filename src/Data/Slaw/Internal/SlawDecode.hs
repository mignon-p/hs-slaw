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

-- The decode functions take the header oct as a Word64, and they
-- also get an Input which points to the word after the header oct.
-- If they want to signal an error in the header oct itself, use
-- this function, which indicates an error one oct before the
-- current position of the Input.  (Yeah, this is a bit ugly.)
fmtErrPrevOct :: Input -> [String] -> String
fmtErrPrevOct inp ss = fmtErr inp' ss
  where inp' = inp { iOff = iOff inp - 8 }

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
nibInfo NibCons           = NibInfo "cons"       lenContainer decCons
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
lenPro o = do
  checkBits (map (,"") [4..7]) o
  let hi52   = (o .&. 0x0fff_ffff_ffff_ff00) `shiftR` 4
      lo4    = o .&. 0xf
      octLen = (hi52 .|. lo4)
  return (octLen, 0)

decPro :: Oct -> Special -> Input -> Either String Slaw
decPro _ _ inp = do
  (hdr2, inp') <- inp // 1
  let h2bs    = L.toStrict $ iLbs hdr2
      bo      = iBo hdr2
      hdrOct2 = decodeOct bo h2bs
      tb      = testBit hdrOct2
      bigRude = tb 59
      hasIng  = tb 61
      hasDes  = tb 62
  withMore (addCtxPrev inp []) $ checkBits [(63, "nonstandard")] hdrOct2
  let (rudeBytes, rudeOcts) =
        if bigRude
        then let rudeLen = hdrOct2 .&. 0x07ff_ffff_ffff_ffff
             in (rudeLen, (rudeLen + 7) `div` 8)
        else (msb3lsb hdrOct2, 0)
      nOcts = (L.length . iLbs) inp' `div` 8
  (body, rude) <- inp' // fromIntegral nOcts - rudeOcts
  let rb1      = fromIntegral rudeBytes
      rb2      = fromIntegral rudeBytes
      rudeData =
        if bigRude
        then L.take rb1   $ iLbs rude
        else L.fromStrict $ getSpecial bo h2bs rb2
      slawx    = decodeSequence "protein" Nothing 0 body
      barf msg = mkErr body [msg]
  (des, ing) <- case (hasDes, hasIng, slawx) of
                  (True, True,  (d:i:_)) -> Right (Just d, Just i)
                  (True, False, (d:_  )) -> Right (Just d, Nothing)
                  (False, True, (i:_  )) -> Right (Nothing, Just i)
                  (True, True,  [_]    ) -> barf "no ingests"
                  (True, _,     []     ) -> barf "no descrips"
                  (False, True, []     ) -> barf "no ingests"
                  _                      -> Right (Nothing, Nothing)
  return $ SlawProteinRude des ing rudeData

lenSym :: Oct -> Either String (Word64, Word)
lenSym o = do
  checkBits (map (,"") [56..59]) o
  return (1, 0)

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
lenWstr o = return (1, msb3lsb o)

decWstr :: Oct -> Special -> Input -> Either String Slaw
decWstr o spec inp = do
  withMore (addCtxPrev inp []) $ checkBits stringReservedBit o
  (return . SlawString . trimNul . L.fromStrict) spec

decList :: Oct -> Special -> Input -> Either String Slaw
decList o _ inp = SlawList <$> decodeSequence0 True "list" o inp

decodeSequence0 :: Bool -> String -> Oct -> Input -> Either String [Slaw]
decodeSequence0 check what o inp = do
  let nElems0 = penultimateNibble o
  (nElems, inp') <- if nElems0 < 15
                    then return (nElems0, inp)
                    else longerLen inp
  let nElems' = if check then Just nElems else Nothing
  return $ decodeSequence what nElems' 0 inp'

decodeSequence :: String -> Maybe Word64 -> Word64 -> Input -> [Slaw]
decodeSequence what nElems !idx inp =
  let exhausted = (L.null . iLbs) inp
      finished  = maybe exhausted (idx >=) nElems
      slawE xs  = [ (SlawError . fmtErr inp) xs ]
      nElems'   = maybe "???" show nElems
  in case (exhausted, finished) of
       (True, True)  -> []
       (False, True) -> slawE [ "There should have been "
                              , nElems'
                              , " elements in "
                              , what
                              , ", but there are more than that"
                              ]
       (True, False) -> slawE [ "Expected "
                              , nElems'
                              , " elements in "
                              , what
                              , ", but only got "
                              , show idx
                              ]
       (False, False) ->
         case decodeSlaw1 inp of
           Left msg        -> [SlawError msg]
           Right (s, inp') ->
             s : decodeSequence what nElems (idx + 1) inp'

longerLen :: Input -> Either String (Word64, Input)
longerLen inp = do
  (inp1, inp2) <- inp // 1
  let bs = (L.toStrict . iLbs) inp1
  return (decodeOct (iBo inp1) bs, inp2)

decMap :: Oct -> Special -> Input -> Either String Slaw
decMap o _ inp = do
  elems <- decodeSequence0 True "map" o inp
  return $ SlawMap $ zipWith cons2Pair elems [0..]

cons2Pair :: Slaw -> Word64 -> (Slaw, Slaw)
cons2Pair (SlawCons car cdr) _ = (car, cdr)
cons2Pair s@(SlawError _)    _ = (s,   s  )
cons2Pair s                  n = (SlawError msg, s)
  where msg = printf "Element %u of map was not a cons" n

decCons :: Oct -> Special -> Input -> Either String Slaw
decCons o _ inp = do
  let nElems   = penultimateNibble o
      butMsg   = "Cons should have 2 elements, but "
      elements = " elements"
  withMore (addCtx inp []) $ do
    when (nElems /= (2 :: Int)) $ do
      Left $ concat [ butMsg
                    , "claims to have "
                    , show nElems
                    , elements
                    ]
    elems <- decodeSequence0 False "cons" o inp
    case elems of
      [car, cdr] -> return $ SlawCons car cdr
      _ -> Left $ concat [ butMsg
                         , "actually found "
                         , show (length elems)
                         , elements
                         ]

lenContainer :: Oct -> Either String (Word64, Word)
lenContainer o = Right (lo56 o, 0)

lenStr :: Oct -> Either String (Word64, Word)
lenStr o = return (lo56 o, 0)

decStr :: Oct -> Special -> Input -> Either String Slaw
decStr o _ inp = do
  let padding = msb3lsb o
      lbs     = L.dropEnd padding (iLbs inp)
  withMore (addCtxPrev inp []) $ checkBits stringReservedBit o
  (return . SlawString . trimNul) lbs

lenNum :: Bool -> Oct -> Either String (Word64, Word)
lenNum isArray o =
  let bsize   = getBsize o
      breadth = getBreadth isArray o
      byteLen = bsize * breadth
      octLen  = 1 + (byteLen + 7) `div` 8
  in if byteLen <= 4 && not isArray
     then Right (1, fromIntegral byteLen)
     else Right (octLen, 0)

getBsize :: Oct -> Word64
getBsize o = 1 + (0xff .&. (o `shiftR` 46))

getBreadth :: Bool -> Oct -> Word64
getBreadth True  = (.&. 0x0000_3fff_ffff_ffff)
getBreadth False = const 1

getComplex :: Oct -> Bool
getComplex = (`testBit` 57)

getVtype :: Oct -> VectorType
getVtype o = toEnum $ fromIntegral $ 7 .&. (o `shiftL` 54)

getElemSize :: Oct -> Int
getElemSize o = 1 `shiftL` fromIntegral (3 .&. (o `shiftL` 58))

decNum :: (Bool, NumTyp)
       -> Oct
       -> Special
       -> Input
       -> Either String Slaw
decNum (isArray, typ) o special inp = do
  withMore (addCtxPrev inp []) $ do
    let bsize     = getBsize o
        breadth   = getBreadth isArray o
        byteLen   = bsize * breadth
        isComplex = getComplex  o
        vType     = getVtype    o
        elemSize  = getElemSize o -- size in bytes (1, 2, 4, or 8)
        nf        = NumericFormat isArray isComplex vType
        bsize'    = computeBsize nf elemSize
    when (bsize /= fromIntegral bsize') $
      Left $ concat [ "Bsize is "
                    , show bsize
                    , ", but according to isComplex="
                    , show isComplex
                    , " vType="
                    , show vType
                    , " elemSize="
                    , show elemSize
                    , ", bsize should be "
                    , show bsize'
                    ]
    nt <- unclassifyNumeric (typ, elemSize)
    let bl = fromIntegral byteLen
        bs = if | byteLen == 0                -> B.empty
                | byteLen <= 4 && not isArray -> special
                | otherwise -> (L.toStrict . L.take bl . iLbs) inp
        nd = restoreNumeric (iBo inp) nt bs
    return $ SlawNumeric nf nd

lenUnk :: Oct -> Either String (Word64, Word)
lenUnk = Left . unkMsg

decUnk :: Oct -> Special -> Input -> Either String Slaw
decUnk o _ inp = mkErr inp [unkMsg o]

unkMsg :: Oct -> String
unkMsg o = printf "Most-significant nibble is reserved value 0x%x" nib
  where nib = o `shiftR` 60

stringReservedBit :: [(Int, String)]
stringReservedBit = [(59, "reserved")]

-- the 4 least significant bits of the most significant byte of an oct
penultimateNibble :: Integral a => Oct -> a
penultimateNibble = fromIntegral . (.&. 0xf) . hi8

-- the 3 least significant bits of the most significant byte of an oct
msb3lsb :: Integral a => Oct -> a
msb3lsb = fromIntegral . (.&. 7) . hi8

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

unclassifyNumeric :: (NumTyp, Int) -> Either String NumericType
unclassifyNumeric pair@(t, nb) =
  case pair `M.lookup` numMap of
    Just nt -> Right nt
    Nothing -> Left $ concat msg
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

addCtxPrev :: Input -> [String] -> String -> String
addCtxPrev inp ss s = fmtErrPrevOct inp (ss ++ [s])
