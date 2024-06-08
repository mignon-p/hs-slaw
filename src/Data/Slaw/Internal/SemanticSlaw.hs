{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}

module Data.Slaw.Internal.SemanticSlaw
  ( Semantic(Semantic)
  ) where

import Control.DeepSeq
import Data.Bifunctor
import Data.Hashable
import qualified Data.Map.Strict      as M
-- import Data.Ratio
import qualified Data.Text            as T
import qualified Data.Vector.Storable as S
import GHC.Generics (Generic)

import Data.Slaw.Internal.Helpers
import Data.Slaw.Internal.SlawType
import Data.Slaw.Internal.String
import Data.Slaw.Internal.Util

newtype Semantic = Semantic1 SemWrap
                   deriving newtype (Eq, Ord, Show, NFData, Hashable)

pattern Semantic :: Slaw -> Semantic
pattern Semantic a <- Semantic1 (SemWrap { swOrig = a }) where
  Semantic a = Semantic1 (semWrap fromUtf8 a)

data SemSlaw = SemNil
             | SemBool    !Bool
             | SemSymbol  !Symbol
             | SemNumeric !NumericFormat [Rational]
             | SemString  T.Text
             | SemCons    SemSlaw SemSlaw
             | SemList    [SemSlaw]
             | SemMap     [(SemSlaw, SemSlaw)]
             | SemProtein SemSlaw SemSlaw RudeData
             | SemError
             deriving (Eq, Ord, Show, Generic, NFData, Hashable)

data SemWrap = SemWrap
  { swOrig :: Slaw
  , swSem  :: SemSlaw
  , swHash :: Int
  }

instance Eq SemWrap where
  x == y = swSem x == swSem y

instance Ord SemWrap where
  x `compare` y = swSem x ?? swSem y

instance Show SemWrap where
  showsPrec d = showsPrec d . swOrig

instance Hashable SemWrap where
  hash           = swHash
  hashWithSalt s = hashWithSalt s . swHash

instance NFData SemWrap where
  rnf x = swOrig x `deepseq` swSem x `deepseq` rnf (swHash x)

semWrap :: (Utf8Str -> T.Text) -> Slaw -> SemWrap
semWrap f slaw = SemWrap
  { swOrig = slaw
  , swSem  = sem
  , swHash = hash sem
  }
  where
    sem = mkSemSlaw f slaw

mkSemSlaw :: (Utf8Str -> T.Text) -> Slaw -> SemSlaw
mkSemSlaw _ SlawNil             = SemNil
mkSemSlaw _ (SlawBool b)        = SemBool    b
mkSemSlaw _ (SlawSymbol sym)    = SemSymbol  sym
mkSemSlaw _ (SlawNumeric nf nd) = SemNumeric nf $ ndToRats nd
mkSemSlaw f (SlawString utf8)   = SemString  $ f utf8
mkSemSlaw f (SlawCons car cdr)  =
  SemCons (mkSemSlaw f car) (mkSemSlaw f cdr)
mkSemSlaw f (SlawList lst)      = SemList    $ map (mkSemSlaw f) lst
mkSemSlaw f (SlawMap pairs)     = SemMap     $ mkSemMap f pairs
mkSemSlaw f (SlawProtein d i r) = SemProtein d' i' r
  where d' = mkSemSlaw f $ d ?> SlawList []
        i' = mkSemSlaw f $ i ?> SlawMap  []
mkSemSlaw _ (SlawError _ _)     = SemError

mkSemMap :: (Utf8Str -> T.Text)
         -> [(Slaw, Slaw)]
         -> [(SemSlaw, SemSlaw)]
mkSemMap f = M.toList . M.fromList . map (bimap g g)
  where g = mkSemSlaw f

ndToRats :: NumericData -> [Rational]
ndToRats = fromNumericData (map toRational . S.toList)
