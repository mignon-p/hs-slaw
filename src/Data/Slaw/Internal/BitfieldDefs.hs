{-|
Module      : Data.Slaw.Internal.BitfieldDefs
Description : Definitions of specific bitfields for slaw-v2
Copyright   : © Mignon Pelletier, 2024
License     : MIT
Maintainer  : code@funwithsoftware.org
Portability : GHC
-}

module Data.Slaw.Internal.BitfieldDefs
  ( bfNybHi
  , bfNyb2nd
  , bfStrLen
  , bfStrRsvd
  , bfCount
  , bfFloat
  , bfUnsigned
  , bfNumSize
  , bfComplex
  , bfVector
  , bfBsize
  , bfBreadth
  , bfNonStd
  , bfDescrips
  , bfIngests
  , bfFuture
  , bfBigRude
  , bfWeeRudeLen
  , bfBigRudeLen
  , bfProteinLo
  , bfMagic
  , bfVersion
  , bfType
  , bfBigEndian
  ) where

import Data.Slaw.Internal.Bitfield

                              -- general --

{-# INLINE bfNybHi #-}
bfNybHi :: Bitfield
bfNybHi = Bitfield 60 4

{-# INLINE bfNyb2nd #-}
bfNyb2nd :: Bitfield
bfNyb2nd = Bitfield 56 4

                               -- string --

{-# INLINE bfStrLen #-}
bfStrLen :: Bitfield
bfStrLen = Bitfield 56 3

{-# INLINE bfStrRsvd #-}
bfStrRsvd :: Bitfield
bfStrRsvd = Bitfield 59 1

                              -- list/map --

{-# INLINE bfCount #-}
bfCount :: Bitfield
bfCount = Bitfield 56 4

                              -- numeric --

{-# INLINE bfFloat #-}
bfFloat :: Bitfield
bfFloat = Bitfield 61 1

{-# INLINE bfUnsigned #-}
bfUnsigned :: Bitfield
bfUnsigned = Bitfield 60 1

{-# INLINE bfNumSize #-}
bfNumSize :: Bitfield
bfNumSize = Bitfield 58 2

{-# INLINE bfComplex #-}
bfComplex :: Bitfield
bfComplex = Bitfield 57 1

{-# INLINE bfVector #-}
bfVector :: Bitfield
bfVector = Bitfield 54 3

{-# INLINE bfBsize #-}
bfBsize :: Bitfield
bfBsize = Bitfield 46 8

{-# INLINE bfBreadth #-}
bfBreadth :: Bitfield
bfBreadth = Bitfield 0 46

                              -- proteins --

{-# INLINE bfNonStd #-}
bfNonStd :: Bitfield
bfNonStd = Bitfield 63 1

{-# INLINE bfDescrips #-}
bfDescrips :: Bitfield
bfDescrips = Bitfield 62 1

{-# INLINE bfIngests #-}
bfIngests :: Bitfield
bfIngests = Bitfield 61 1

{-# INLINE bfFuture #-}
bfFuture :: Bitfield
bfFuture = Bitfield 60 1

{-# INLINE bfBigRude #-}
bfBigRude :: Bitfield
bfBigRude = Bitfield 59 1

{-# INLINE bfWeeRudeLen #-}
bfWeeRudeLen :: Bitfield
bfWeeRudeLen = Bitfield 56 3

{-# INLINE bfBigRudeLen #-}
bfBigRudeLen :: Bitfield
bfBigRudeLen = Bitfield 0 59

{-# INLINE bfProteinLo #-}
bfProteinLo :: Bitfield
bfProteinLo = Bitfield 4 4

                      -- binary slaw file header --

{-# INLINE bfMagic #-}
bfMagic :: Bitfield
bfMagic = Bitfield 32 32

{-# INLINE bfVersion #-}
bfVersion :: Bitfield
bfVersion = Bitfield 24 8

{-# INLINE bfType #-}
bfType :: Bitfield
bfType = Bitfield 16 8

{-# INLINE bfBigEndian #-}
bfBigEndian :: Bitfield
bfBigEndian = Bitfield 0 1
