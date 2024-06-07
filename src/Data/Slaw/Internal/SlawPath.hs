module Data.Slaw.Internal.SlawPath
  ( slawPath
  , slawPath_m
  , slawPath_es
  , slawPath_ee
  , (!)
  , (!?)
  ) where

import Control.Exception
import Data.Bifunctor
import Data.List
import Data.Maybe
import qualified Data.Text                as T
import GHC.Stack
import Text.Read

import Data.Slaw.Internal.Exception
import Data.Slaw.Internal.Helpers
import Data.Slaw.Internal.SlawConvert
import Data.Slaw.Internal.SlawEncode
import Data.Slaw.Internal.SlawType
import Data.Slaw.Internal.String
import Data.Slaw.Internal.Util

infixl 9 !
infixl 9 !?

(!) :: (HasCallStack, FromSlaw a) => Slaw -> T.Text -> a
(!) s path =
  case slawPathCvt s path of
    Left exc -> throw $ exc { peCallstack = Just callStack }
    Right x  -> x

(!?) :: FromSlaw a => Slaw -> T.Text -> Maybe a
s !? path = eth2mby $ slawPathCvt s path

slawPathCvt :: FromSlaw a
            => Slaw
            -> T.Text
            -> Either PlasmaException a
slawPathCvt s path = slawPath_ee' s path >>= fromSlaw

slawPath :: HasCallStack => Slaw -> T.Text -> Slaw
slawPath s path =
  case slawPath_ee' s path of
    Left exc -> throw $ exc { peCallstack = Just callStack }
    Right x  -> x

slawPath_m :: Slaw -> T.Text -> Maybe Slaw
slawPath_m s = eth2mby . slawPath_ee' s

slawPath_es :: Slaw -> T.Text -> Either String Slaw
slawPath_es s = first (displayPlasmaException False) . slawPath_ee' s

slawPath_ee :: Slaw -> T.Text -> Either PlasmaException Slaw
slawPath_ee s =
  first (\e -> e { peCallstack = Just callStack }) . slawPath_ee' s

slawPath_ee' :: Slaw -> T.Text -> Either PlasmaException Slaw
slawPath_ee' s path =
  first (mapExc path) $ slawPath1 0 s $ T.splitOn "/" path

mapExc :: T.Text -> (Int, Int, String) -> PlasmaException
mapExc path (pos, len, msg) =
  notFoundErr $ intercalate "\n" [msg, stdIndent ++ ln2, stdIndent ++ ln3]
  where
    ln2 = T.unpack path
    ln3 = replicate pos ' ' ++ replicate len '^'

slawPath1 :: Int -> Slaw -> [T.Text] -> Either (Int, Int, String) Slaw
slawPath1 _    s []       = Right s
slawPath1 !pos s (k:rest) =
  let kLen = T.length k
  in case fetchSlaw s k of
       Just s' -> slawPath1 (pos + kLen + 1) s' rest
       Nothing -> Left (pos, kLen, concat [ "Could not find "
                                          , show k
                                          , " in "
                                          , describeSlaw s
                                          , ":"
                                          ])

fetchSlaw :: Slaw -> T.Text -> Maybe Slaw
fetchSlaw (SlawProtein des ing rude) = fetchProtein des ing rude
fetchSlaw (SlawList    lst         ) = fetchList    lst
fetchSlaw (SlawMap     pairs       ) = fetchMap     pairs
fetchSlaw (SlawCons    car cdr     ) = fetchCons    car cdr
fetchSlaw (SlawNumeric nf  nd      ) = fetchNumeric nf nd
fetchSlaw _                          = const        Nothing

findMatch :: [([T.Text], Slaw)] -> T.Text -> Maybe Slaw
findMatch pairs = findMatch' pairs . T.toLower

findMatch' :: [([T.Text], Slaw)] -> T.Text -> Maybe Slaw
findMatch' [] _ = Nothing
findMatch' ((keys, s):rest) k
  | k `elem` keys = Just s
  | otherwise     = findMatch' rest k

lw :: T.Text -> [T.Text]
lw = T.words . T.toLower

fetchCons :: Slaw -> Slaw -> T.Text -> Maybe Slaw
fetchCons car cdr = findMatch
  [ (lw "a car 0", car)
  , (lw "d cdr 1", cdr)
  ]

fetchProtein :: Maybe Slaw
             -> Maybe Slaw
             -> RudeData
             -> T.Text
             -> Maybe Slaw
fetchProtein des ing rude = findMatch
  [ (lw "d des  descrips 0", des ?> SlawList [])
  , (lw "i ing  ingests  1", ing ?> SlawMap  [])
  , (lw "r rude rudeData 2", toSlaw rude)
  ]

fetchList :: [Slaw] -> T.Text -> Maybe Slaw
fetchList ss k = do
  idx <- readMaybe (T.unpack k)
  let idx' = if idx < 0
             then idx + length ss
             else idx
  listToMaybe $ if idx' < 0 then [] else drop idx' ss

fetchMap :: [(Slaw, Slaw)] -> T.Text -> Maybe Slaw
fetchMap pairs k = fmap snd $ find (fm1 utf8 num) pairs
  where utf8 = toUtf8 k
        num  = readMaybe $ T.unpack k

fm1 :: Utf8Str -> Maybe Integer -> (Slaw, Slaw) -> Bool
fm1 utf8a _   (SlawString  utf8b, _) = utf8a == utf8b
fm1 _     num (SlawNumeric _  nd, _) = fm1num num nd
fm1 _     _   _                      = False

fm1num :: Maybe Integer -> NumericData -> Bool
fm1num Nothing    _  = False
fm1num (Just num) nd =
  case numToList nd of
    [ElemInt n] -> n == num
    _           -> False

fetchNumeric :: NumericFormat -> NumericData -> T.Text -> Maybe Slaw
fetchNumeric nf nd k
  | nfArray nf              = fnArray   nf nd k
  | nfVector nf /= VtScalar = fnVector  nf nd k
  | nfComplex nf            = fnComplex nf nd k
  | otherwise               = Nothing

fn1 :: [(T.Text, Int)]
    -> Int
    -> NumericData
    -> T.Text
    -> Maybe NumericData
fn1 pairs chunkSize nd k = do
  let len = lengthNumericData nd `div` chunkSize
  idx <- fnIndex pairs len k
  return $ sliceNumericData (idx * chunkSize) chunkSize nd

fn2 :: [(T.Text, Int)]
    -> Int
    -> NumericFormat
    -> NumericData
    -> T.Text
    -> Maybe Slaw
fn2 pairs chunkSize nf nd = fmap f . fn1 pairs chunkSize nd
  where f nd' = SlawNumeric nf nd'

fnIndex :: [(T.Text, Int)] -> Int -> T.Text -> Maybe Int
fnIndex pairs len k = do
  idx <- case readMaybe (T.unpack k) of
           Nothing -> lookup (T.toLower k) pairs
           Just n  -> Just n
  let idx' = if idx < 0 then idx + len else idx
  if idx' >= 0 && idx' < len
    then Just idx'
    else Nothing

fnArray :: NumericFormat -> NumericData -> T.Text -> Maybe Slaw
fnArray nf  = fn2 [] (numericFormatSize nf) nf'
  where nf' = nf { nfArray = False }

fnVector :: NumericFormat -> NumericData -> T.Text -> Maybe Slaw
fnVector nf = fn2 names scalarSize nf'
  where nf'        = nf { nfVector = VtScalar }
        scalarSize = if nfComplex nf then 2 else 1
        names      = zip (map T.singleton "xyzw") [0..]

fnComplex :: NumericFormat -> NumericData -> T.Text -> Maybe Slaw
fnComplex nf = fn2 names 1 nf'
  where nf'   = nf { nfComplex = False }
        names = concat [ map (,0) (T.words "r re real")
                       , map (,1) (T.words "i im imaginary")
                       ]
