{-# language DuplicateRecordFields #-}

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

import qualified Data.Primitive as PM
import qualified Data.Primitive.Unlifted.Array as PM

data UnliftedVector a = UnliftedVector
  { array :: !(UnliftedArray a)
  , offset :: !Int
  , length :: !Int
  }

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


