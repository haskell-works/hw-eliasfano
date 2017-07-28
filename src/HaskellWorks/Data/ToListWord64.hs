module HaskellWorks.Data.ToListWord64
  ( ToListWord64(..)
  ) where

import Data.Word

class ToListWord64 a where
  toListWord64 :: a -> [Word64]
