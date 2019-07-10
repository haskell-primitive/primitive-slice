{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}

module Data.Primitive.Slice
  ( -- * Types
    UnliftedVector(..)
  , MutableUnliftedVector(..)
  , SmallVector(..)
  , SmallMutableVector(..)
    -- * Conversion
  , unslicedUnliftedVector
  , unslicedSmallVector
  ) where

import Prelude hiding (length)

import Data.Primitive (SmallArray,SmallMutableArray)
import Data.Primitive.Unlifted.Array (UnliftedArray,MutableUnliftedArray)
import Data.Primitive.Unlifted.Class (PrimUnlifted)
import GHC.Exts (IsList)

import qualified GHC.Exts as Exts
import qualified Data.Primitive as PM
import qualified Data.Primitive.Unlifted.Array as PM

data UnliftedVector a = UnliftedVector
  { array :: !(UnliftedArray a)
  , offset :: !Int
  , length :: !Int
  }

instance PrimUnlifted a => IsList (UnliftedVector a) where
  type Item (UnliftedVector a) = a
  fromList = unslicedUnliftedVector . Exts.fromList
  fromListN n = unslicedUnliftedVector . Exts.fromListN n
  toList = foldrUnliftedVector (:) []

data MutableUnliftedVector s a = MutableUnliftedVector
  { array :: !(MutableUnliftedArray s a)
  , offset :: !Int
  , length :: !Int
  }

data SmallVector a = SmallVector
  { array :: !(SmallArray a)
  , offset :: !Int
  , length :: !Int
  }

data SmallMutableVector s a = SmallMutableVector
  { array :: !(SmallMutableArray s a)
  , offset :: !Int
  , length :: !Int
  }

unslicedUnliftedVector :: UnliftedArray a -> UnliftedVector a
unslicedUnliftedVector x = UnliftedVector
  { array = x
  , offset = 0
  , length = PM.sizeofUnliftedArray x
  }

unslicedSmallVector :: SmallArray a -> SmallVector a
unslicedSmallVector x = SmallVector
  { array = x
  , offset = 0
  , length = PM.sizeofSmallArray x
  }

foldrUnliftedVector :: forall a b. PrimUnlifted a => (a -> b -> b) -> b -> UnliftedVector a -> b
{-# INLINE foldrUnliftedVector #-}
foldrUnliftedVector f z (UnliftedVector arr off0 len) = go off0
  where
    !end = len + off0
    go !i
      | end > i = f (PM.indexUnliftedArray arr i) (go (i+1))
      | otherwise = z
