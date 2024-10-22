{-|
Module      : Data.Slaw.Internal.Merge
Description : Typeclass for taking the union of two things
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module Data.Slaw.Internal.Merge
  ( Merge(..)
  , prefLeftAsSlaw
  , prefRightAsSlaw
  ) where

import Control.Applicative
import qualified Data.ByteString.Lazy as L
import Data.Containers.ListUtils (nubOrd)

import Data.Slaw.Internal.OptionTypes (coerceToMap)
import Data.Slaw.Internal.SlawConvert
import Data.Slaw.Internal.SlawType
import Data.Slaw.Internal.Util

class Merge a where
  {-# MINIMAL (prefLeft | prefRight) #-}

  prefLeft :: a -> a -> a
  prefLeft = flip prefRight

  prefRight :: a -> a -> a
  prefRight = flip prefLeft

instance Merge (Maybe a) where
  prefLeft = (<|>)

instance Merge Slaw where
  prefLeft  = prefSlaw fst preferLeft
  prefRight = prefSlaw snd preferRight

type SlawPairs = [(Slaw, Slaw)]

prefSlaw
  :: ((Slaw, Slaw) -> Slaw)
  -> (SlawPairs -> SlawPairs -> SlawPairs)
  -> Slaw
  -> Slaw
  -> Slaw
prefSlaw pick merge s1 s2 =
  let sx = pick (s1, s2)
  in case (coerceToMap s1, coerceToMap s2) of
       (SlawMap m1, SlawMap m2) ->
         let mm = m1 `merge` m2
         in case sx of
              SlawProtein {} -> prefProt pick s1 s2 mm
              _              -> SlawMap mm
       (SlawMap _ , _         ) -> s1
       (_         , SlawMap _ ) -> s2
       _                        ->
         case s1 <> s2 of
           SlawError {} -> sx
           ss           -> ss

prefProt
  :: ((Slaw, Slaw) -> Slaw)
  -> Slaw
  -> Slaw
  -> SlawPairs
  -> Slaw
prefProt pick (SlawProtein d1 _ r1) (SlawProtein d2 _ r2) mm =
  let dd = prefDescrips pick (d1 ?> ee) (d2 ?> ee)
      rr = prefRude     pick r1 r2
      ee = SlawList []
  in SlawProtein dd (Just $ SlawMap mm) rr
prefProt _ p1@(SlawProtein {}) _ mm =
  p1 { slawIngests = (Just . SlawMap) mm }
prefProt _ _ p2@(SlawProtein {}) mm =
  p2 { slawIngests = (Just . SlawMap) mm }
prefProt _ _ _ mm = SlawMap mm

prefDescrips
  :: ((Slaw, Slaw) -> Slaw)
  -> Slaw
  -> Slaw
  -> Maybe Slaw
prefDescrips _ (SlawList d1) (SlawList d2) =
  case nubOrd (d1 ++ d2) of
    [] -> Nothing
    dd -> Just $ SlawList dd
prefDescrips pick d1 d2 = Just $ pick (d1, d2)

prefRude
  :: ((Slaw, Slaw) -> Slaw)
  -> RudeData
  -> RudeData
  -> RudeData
prefRude pick r1 r2
  | have1 && have2 = slawRudeData $ pick (p1, p2)
  | have1          = r1
  | otherwise      = r2
  where
    have1 = not $ L.null r1
    have2 = not $ L.null r2
    p1    = SlawProtein Nothing Nothing r1
    p2    = SlawProtein Nothing Nothing r2

prefLeftAsSlaw :: (ToSlaw a, FromSlaw a) => a -> a -> a
prefLeftAsSlaw x1 x2 = ŝm (prefLeft (š x1) (š x2)) ?> x1

prefRightAsSlaw :: (ToSlaw a, FromSlaw a) => a -> a -> a
prefRightAsSlaw x1 x2 = ŝm (prefRight (š x1) (š x2)) ?> x2
