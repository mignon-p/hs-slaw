{-# LANGUAGE ScopedTypeVariables        #-}

module Data.Slaw.Internal.SlawConvert
  ( FromSlaw(..)
  , ToSlaw(..)
  , š
  , ŝ
  , ŝm
  , ŝes
  , ŝee
  , (?:)
  , handleOthers
  , Protein(..)
  ) where

import Control.DeepSeq
import Control.Exception
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as L
import qualified Data.ByteString.Short    as SBS
import Data.Char
import Data.Default.Class
import Data.Either
import Data.Hashable
import qualified Data.HashMap.Strict      as HM
-- import Data.Int
-- import qualified Data.IntMap.Strict       as IM
import Data.List
import qualified Data.Map.Strict          as M
import Data.String
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import qualified Data.Vector.Storable     as S
-- import Data.Word
import Foreign.Storable
import GHC.Generics (Generic)
import GHC.Stack
-- import System.IO.Unsafe (unsafePerformIO)
import Text.Read

import Data.Slaw.Internal.Exception
import Data.Slaw.Internal.SlawEncode
import Data.Slaw.Internal.SlawType
import Data.Slaw.Internal.String
import Data.Slaw.Internal.Util

---- FromSlaw and ToSlaw classes

class FromSlaw a where
  fromSlaw :: Slaw -> Either PlasmaException a

  listFromSlaw :: Slaw -> Either PlasmaException [a]
  listFromSlaw = defaultListFromSlaw

  -- Name of type we are converting to.
  -- (Used in error messages.)
  fsName :: a -> String

class ToSlaw a where
  toSlaw :: a -> Slaw

  listToSlaw :: [a] -> Slaw
  listToSlaw = defaultListToSlaw

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
          Left exc -> Left $ displayPlasmaException False exc
          Right x  -> Right x

-- "from slaw, either exception"
{-# INLINE ŝee #-}
ŝee :: (HasCallStack, FromSlaw a) => Slaw -> Either PlasmaException a
ŝee = mapLeft f . fromSlaw
  where cs  = Just callStack
        f e = e { peCallstack = cs }

-- from slaw, with default value
{-# INLINABLE (?:) #-}
(?:) :: FromSlaw a => Slaw -> a -> a
s ?: dflt = case fromSlaw s of
              Left  _ -> dflt
              Right x -> x

---- helper functions for implementing ToSlaw/FromSlaw

handleOthers :: forall a. (FromSlaw a)
             => Slaw
             -> Either PlasmaException a
handleOthers (SlawError msg loc)
  | typeMismatchPfx `isPrefixOf` msg = Left $ typeMismatch' msg loc
  | otherwise                        = Left $ corruptSlaw   msg loc
handleOthers slaw = Left $ typeMismatch msg
  where msg = slaw `cantCoerce` fsName (undefined :: a)

because :: String -> [PlasmaException] -> Either PlasmaException a
because msg reasons = (Left . typeMismatch) msg'
  where msg'     = concat [ msg, ", because:\n", reasons']
        reasons0 = map (indentLines "  " . peMessage) reasons
        reasons' = intercalate "\nand:\n" reasons0

cantCoerce :: Slaw -> String -> String
cantCoerce slaw other = describeSlaw slaw `cantCoerce1` other

cantCoerce1 :: String -> String -> String
cantCoerce1 desc other = concat ["Can't coerce ", desc, " to ", other]

defaultListToSlaw :: ToSlaw a => [a] -> Slaw
defaultListToSlaw = SlawList . map toSlaw

defaultListFromSlaw :: forall a. (FromSlaw a)
                    => Slaw
                    -> Either PlasmaException [a]
defaultListFromSlaw SlawNil       = Right []
defaultListFromSlaw s@(SlawList ss) =
  let msg = s `cantCoerce` fsName (undefined :: a)
  in fromSlawList msg ss
defaultListFromSlaw s@(SlawMap pairs) =
  let msg    = s `cantCoerce` fsName (undefined :: a)
      conses = map (uncurry SlawCons) pairs
  in fromSlawList msg conses
defaultListFromSlaw s@(SlawCons car cdr) =
  let msg = s `cantCoerce` fsName (undefined :: a)
  in fromSlawList msg [car, cdr]
defaultListFromSlaw s@(SlawNumeric nf nd) =
  let msg = s `cantCoerce` fsName (undefined :: a)
  in case numericArrayToList nf nd of
       Nothing         -> Left $ typeMismatch msg
       Just (nf', nds) -> fromSlawList msg $ map (SlawNumeric nf') nds
defaultListFromSlaw s             = handleOthers s

fromSlawList :: FromSlaw a
             => String
             -> [Slaw]
             -> Either PlasmaException [a]
fromSlawList msg ss =
  case partitionEithers (map fromSlaw ss) of
    ([],      []) -> Right []
    ((err:_), []) -> msg `because` [err]
    (_,       xs) -> Right xs

numericArrayToList :: NumericFormat
                   -> NumericData
                   -> Maybe (NumericFormat, [NumericData])
numericArrayToList nf nd = if isArray then Just (nf', nds) else Nothing
  where
    isArray  = nfArray nf
    nf'      = nf { nfArray = False }
    stepSize = computeBsize nf 1
    nds      = chunkNumericData stepSize nd

chunkNumericData :: Int -> NumericData -> [NumericData]
chunkNumericData n (NumInt8   v) = map NumInt8   $ chunkArray n v
chunkNumericData n (NumInt16  v) = map NumInt16  $ chunkArray n v
chunkNumericData n (NumInt32  v) = map NumInt32  $ chunkArray n v
chunkNumericData n (NumInt64  v) = map NumInt64  $ chunkArray n v
chunkNumericData n (NumUnt8   v) = map NumUnt8   $ chunkArray n v
chunkNumericData n (NumUnt16  v) = map NumUnt16  $ chunkArray n v
chunkNumericData n (NumUnt32  v) = map NumUnt32  $ chunkArray n v
chunkNumericData n (NumUnt64  v) = map NumUnt64  $ chunkArray n v
chunkNumericData n (NumFloat  v) = map NumFloat  $ chunkArray n v
chunkNumericData n (NumDouble v) = map NumDouble $ chunkArray n v

chunkArray :: Storable a
           => Int
           -> S.Vector a
           -> [S.Vector a]
chunkArray stepSize v = v0 : chunkArray stepSize vRest
  where (v0, vRest) = S.splitAt stepSize v

slawFromString :: TextClass a => a -> Slaw
slawFromString = SlawString . toUtf8

slawToString :: (FromSlaw a, TextClass a)
             => Slaw
             -> Either PlasmaException a
slawToString (SlawBool b)   = (Right . fromString . map toLower . show) b
slawToString SlawNil        = (Right . fromText) "nil"
slawToString (SlawString u) = (Right . fromUtf8) u
slawToString s@(SlawNumeric _ nd) = numToStr     s nd
slawToString s                    = handleOthers s

numToStr :: (FromSlaw a, TextClass a)
         => Slaw
         -> NumericData
         -> Either PlasmaException a
numToStr s (NumInt8   v) = numToStr1 s v
numToStr s (NumInt16  v) = numToStr1 s v
numToStr s (NumInt32  v) = numToStr1 s v
numToStr s (NumInt64  v) = numToStr1 s v
numToStr s (NumUnt8   v) = numToStr1 s v
numToStr s (NumUnt16  v) = numToStr1 s v
numToStr s (NumUnt32  v) = numToStr1 s v
numToStr s (NumUnt64  v) = numToStr1 s v
numToStr s (NumFloat  v) = numToStr1 s v
numToStr s (NumDouble v) = numToStr1 s v

numToStr1 :: (FromSlaw a, TextClass a, Storable b, Show b)
          => Slaw
          -> S.Vector b
          -> Either PlasmaException a
numToStr1 s v
  | S.length v == 1 = (Right . fromString . show . S.head) v
  | otherwise       = handleOthers s

numToInteger :: NumericData -> Maybe Integer
numToInteger (NumInt8   v) = numToInteger1 v
numToInteger (NumInt16  v) = numToInteger1 v
numToInteger (NumInt32  v) = numToInteger1 v
numToInteger (NumInt64  v) = numToInteger1 v
numToInteger (NumUnt8   v) = numToInteger1 v
numToInteger (NumUnt16  v) = numToInteger1 v
numToInteger (NumUnt32  v) = numToInteger1 v
numToInteger (NumUnt64  v) = numToInteger1 v
numToInteger (NumFloat  _) = Nothing
numToInteger (NumDouble _) = Nothing

numToInteger1 :: (Storable a, Integral a) => S.Vector a -> Maybe Integer
numToInteger1 v
  | S.length v == 1 = (Just . toInteger . S.head) v
  | otherwise       = Nothing

nfScalar :: NumericFormat
nfScalar = NumericFormat
  { nfArray   = False
  , nfComplex = False
  , nfVector  = VtScalar
  }

nfScalarArray :: NumericFormat
nfScalarArray = nfScalar { nfArray = True }

slawFromByteString :: ByteStringClass a => a -> Slaw
slawFromByteString x = SlawNumeric nfScalarArray nd
  where bs = toByteString x
        nd = restoreNumeric bo TypUnt8 bs
        -- Byte order should not matter, since we are using
        -- a single-byte type (unt8)
        bo = BigEndian

slawToByteString :: (FromSlaw a, ByteStringClass a)
                 => Slaw
                 -> Either PlasmaException a
slawToByteString (SlawProtein _ _ rude) = Right $ fromLazyByteString rude
slawToByteString (SlawString  lbs     ) = Right $ fromLazyByteString lbs
slawToByteString (SlawNumeric nf nd   )
  | nf == nfScalarArray && nt `elem` [TypInt8, TypUnt8] =
      Right $ fromByteString bs
  where (nt, bs) = extractNumeric bo nd
        -- Again, byte order does not matter for int8/unt8
        bo       = BigEndian
slawToByteString s = handleOthers s

mkTupleName :: [String] -> String
mkTupleName names = "(" ++ intercalate ", " names ++ ")"

pairFromSlaw :: forall a b. (FromSlaw a, FromSlaw b)
             => Slaw
             -> (Slaw, Slaw)
             -> Either PlasmaException (a, b)
pairFromSlaw s pair =
  case pairFromSlaw' pair of
    Left err ->
      let msg = s `cantCoerce` fsName (undefined :: a, undefined :: b)
      in msg `because` [err]
    Right pair' -> Right pair'

pairFromSlaw' :: (FromSlaw a, FromSlaw b)
              => (Slaw, Slaw)
              -> Either PlasmaException (a, b)
pairFromSlaw' (x, y) = do
  x' <- fromSlaw x
  y' <- fromSlaw y
  return (x', y')

slawFromMap :: (ToSlaw a, ToSlaw b) => [(a, b)] -> Slaw
slawFromMap = SlawMap . map f
  where f (x, y) = (toSlaw x, toSlaw y)

slawToMap :: (FromSlaw a, FromSlaw b, FromSlaw z)
          => Slaw
          -> z
          -> Either PlasmaException [(a, b)]
slawToMap s@(SlawMap pairs                         ) = mapFromSlaw s pairs
slawToMap s@(SlawProtein _ (Just (SlawMap pairs)) _) = mapFromSlaw s pairs
slawToMap s@(SlawList ss                           ) = mapFromSlaw s pairs
  where pairs = zip (map toSlaw [(0::Integer)..]) ss
slawToMap s = const (handleOthers s)

mapFromSlaw :: (FromSlaw a, FromSlaw b, FromSlaw z)
            => Slaw
            -> [(Slaw, Slaw)]
            -> z
            -> Either PlasmaException [(a, b)]
mapFromSlaw s pairs dummy =
  let msg = s `cantCoerce` fsName dummy
  in case partitionEithers (map mfs1 pairs) of
       ([],      []) -> Right []
       ((err:_), []) -> msg `because` [err]
       (_,       xs) -> Right xs

mfs1 :: forall a b. (FromSlaw a, FromSlaw b)
     => (Slaw, Slaw)
     -> Either PlasmaException (a, b)
mfs1 pair@(x, y) =
  case pairFromSlaw' pair of
    Left err ->
      let msg = str `cantCoerce1` fsName (undefined :: a, undefined :: b)
          str = mkTupleName $ map describeSlaw [x, y]
      in msg `because` [err]
    Right pair' -> Right pair'

proteinFromSlaw :: Slaw
                -> (Maybe Slaw, Maybe Slaw, L.ByteString)
                -> Either PlasmaException Protein
proteinFromSlaw s (des, ing, rude) =
  let des' = des ?> SlawList []
      ing' = ing ?> SlawMap  []
  in case pairFromSlaw' (des', ing') of
       Left err ->
         let msg = s `cantCoerce` fsName (undefined :: Protein)
         in msg `because` [err]
       Right (des2, ing2) -> Right $ Protein des2 ing2 rude

---- types

data Protein = Protein
  { pDescrips :: [T.Text]
  , pIngests  :: M.Map T.Text Slaw
  , pRudeData :: L.ByteString
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance Default Protein where
  def = Protein [] M.empty L.empty

---- instances

instance FromSlaw Slaw where
  fsName _ = "Slaw"
  fromSlaw = Right

instance ToSlaw Slaw where
  toSlaw = id

instance FromSlaw a => FromSlaw [a] where
  fsName _ = "[" ++ fsName (undefined :: a) ++ "]"
  fromSlaw = listFromSlaw

instance ToSlaw a => ToSlaw [a] where
  toSlaw = listToSlaw

instance FromSlaw Bool where
  fsName _ = "Bool"

  fromSlaw (SlawBool b)     = Right b
  fromSlaw SlawNil          = Right False
  fromSlaw (SlawString lbs)
    | lbs ==~ "false"       = Right False
    | lbs ==~ "true"        = Right True
  fromSlaw s                = handleOthers s

instance ToSlaw Bool where
  toSlaw = SlawBool

instance FromSlaw () where
  fsName _ = "()"

  fromSlaw SlawNil = Right ()
  fromSlaw s       = handleOthers s

instance ToSlaw () where
  toSlaw _ = SlawNil

instance FromSlaw T.Text where
  fsName _ = "Text"
  fromSlaw = slawToString

instance ToSlaw T.Text where
  toSlaw = slawFromString

instance FromSlaw LT.Text where
  fsName _ = "lazy Text"
  fromSlaw = slawToString

instance ToSlaw LT.Text where
  toSlaw = slawFromString

instance FromSlaw Char where
  fsName _ = "Char"

  fromSlaw s@(SlawNumeric _ nd) =
    case numToInteger nd of
      Nothing -> handleOthers s
      Just n
        | n >= 0 && n <= (toInteger . ord) (maxBound :: Char) ->
            (Right . chr . fromInteger) n
        | otherwise -> Left $ typeMismatch $ show n `cantCoerce1` "Char"
  fromSlaw s = handleOthers s

  listFromSlaw = slawToString

instance ToSlaw Char where
  toSlaw c = SlawNumeric nfScalar nd
    where nd = (NumUnt32 . S.singleton . fromIntegral . ord) c

  listToSlaw = slawFromString

instance FromSlaw Integer where
  fsName _ = "Integer"

  fromSlaw s@(SlawNumeric _ nd) =
    case numToInteger nd of
      Nothing -> handleOthers s
      Just n  -> Right n
  fromSlaw (SlawString utf8) =
    let str = fromUtf8 utf8
    in case readMaybe str of
         Just n  -> Right n
         Nothing -> Left $ typeMismatch $ show str `cantCoerce1` "Integer"
  fromSlaw s = handleOthers s

instance ToSlaw Integer where
  toSlaw n =
    case integerToNum n of
      Just nd -> SlawNumeric nfScalar nd
      Nothing -> SlawString $ toUtf8 $ show n

instance FromSlaw B.ByteString where
  fsName _ = "ByteString"
  fromSlaw = slawToByteString

instance ToSlaw B.ByteString where
  toSlaw = slawFromByteString

instance FromSlaw L.ByteString where
  fsName _ = "lazy ByteString"
  fromSlaw = slawToByteString

instance ToSlaw L.ByteString where
  toSlaw = slawFromByteString

instance FromSlaw SBS.ShortByteString where
  fsName _ = "ShortByteString"
  fromSlaw = slawToByteString

instance ToSlaw SBS.ShortByteString where
  toSlaw = slawFromByteString

instance (FromSlaw a, FromSlaw b) => FromSlaw (a, b) where
  fsName _ = mkTupleName [ fsName (undefined :: a)
                         , fsName (undefined :: b)
                         ]
  fromSlaw s@(SlawProtein des ing _) = pairFromSlaw s ( des ?> SlawList []
                                                      , ing ?> SlawMap  []
                                                      )
  fromSlaw s@(SlawCons    car cdr  ) = pairFromSlaw s (car, cdr)
  fromSlaw s@(SlawList    [x, y]   ) = pairFromSlaw s (x,   y  )
  fromSlaw s@(SlawNumeric nf  nd   ) =
    case numericArrayToList nf nd of
      Just (nf', [x, y]) -> pairFromSlaw s ( SlawNumeric nf' x
                                           , SlawNumeric nf' y
                                           )
      _                  -> handleOthers s
  fromSlaw s              = handleOthers s

instance (ToSlaw a, ToSlaw b) => ToSlaw (a, b) where
  toSlaw (car, cdr) = SlawCons (toSlaw car) (toSlaw cdr)

instance (FromSlaw a, FromSlaw b, Ord a) => FromSlaw (M.Map a b) where
  fsName _ = concat [ "Map "
                    , fsName (undefined :: a)
                    , " "
                    , fsName (undefined :: b)
                    ]
  fromSlaw s = mapRight M.fromList $ slawToMap s (M.empty :: M.Map a b)

instance (ToSlaw a, ToSlaw b, Ord a) => ToSlaw (M.Map a b) where
  toSlaw = slawFromMap . M.toList

instance ( FromSlaw a
         , FromSlaw b
         , Hashable a
         ) => FromSlaw (HM.HashMap a b) where
  fsName _ = concat [ "HashMap "
                    , fsName (undefined :: a)
                    , " "
                    , fsName (undefined :: b)
                    ]
  fromSlaw s =
    mapRight HM.fromList $ slawToMap s (HM.empty :: HM.HashMap a b)

instance (ToSlaw a, ToSlaw b, Ord a) => ToSlaw (HM.HashMap a b) where
  toSlaw = slawFromMap . HM.toList

{- uncomment once we have FromSlaw/ToSlaw instances for Int
instance (FromSlaw b) => FromSlaw (IM.IntMap b) where
  fsName _ = concat [ "IntMap "
                    , fsName (undefined :: b)
                    ]
  fromSlaw s =
    mapRight IM.fromList $ slawToMap s (IM.empty :: IM.IntMap b)

instance (ToSlaw b) => ToSlaw (IM.IntMap b) where
  toSlaw = slawFromMap . IM.toList
-}

instance FromSlaw Protein where
  fsName _ = "Protein"

  fromSlaw SlawNil                      = Right def
  fromSlaw s@(SlawProtein des ing rude) =
    proteinFromSlaw s (des, ing, rude)
  fromSlaw s@(SlawMap _)                =
    proteinFromSlaw s (Nothing, Just s, L.empty)
  fromSlaw s@(SlawCons car cdr)         =
    proteinFromSlaw s (Just car, Just cdr, L.empty)
  fromSlaw s                            = handleOthers s

instance ToSlaw Protein where
  toSlaw (Protein des ing rude) =
    let des' = if null   des then Nothing else (Just . toSlaw) des
        ing' = if M.null ing then Nothing else (Just . toSlaw) ing
    in SlawProtein des' ing' rude

instance (FromSlaw a, FromSlaw b) => FromSlaw (Either a b) where
  fsName _ = concat [ "Either "
                    , fsName (undefined :: a)
                    , " "
                    , fsName (undefined :: b)
                    ]

  fromSlaw s =
    case (fromSlaw s, fromSlaw s) of
      (Right x, _      ) -> Right $ Left  x
      (Left  _, Right x) -> Right $ Right x
      (Left e1, Left e2) ->
        let msg = s `cantCoerce` fsName (undefined :: Either a b)
        in msg `because` [e1, e2]

instance (ToSlaw a, ToSlaw b) => ToSlaw (Either a b) where
  toSlaw (Left x ) = toSlaw x
  toSlaw (Right x) = toSlaw x

instance FromSlaw a => FromSlaw (Maybe a) where
  fsName _ = "Maybe " ++ fsName (undefined :: a)

  fromSlaw SlawNil = Right Nothing
  fromSlaw s =
    case fromSlaw s of
      Right x  -> Right x
      Left err ->
        let msg = s `cantCoerce` fsName (Nothing :: Maybe a)
        in msg `because` [err]

instance ToSlaw a => ToSlaw (Maybe a) where
  toSlaw Nothing  = SlawNil
  toSlaw (Just x) = toSlaw x
