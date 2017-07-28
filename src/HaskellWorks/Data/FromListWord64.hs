{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.FromListWord64
  ( FromListWord64(..)
  ) where

import Data.Word

class FromListWord64 a where
  fromListWord64 :: [Word64] -> a
