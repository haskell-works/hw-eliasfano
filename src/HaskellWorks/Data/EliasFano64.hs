{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.EliasFano64
  ( EliasFano64(..)
  , FromEliasFano64(..)
  , ToEliasFano64(..)
  , FromListWord64(..)
  , ToListWord64(..)
  ) where

import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.Log2
import HaskellWorks.Data.EliasFano64.Internal
import HaskellWorks.Data.PackedVector         as PV
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.Take
import Prelude                                hiding (length, take)
import Safe

import qualified Data.Vector.Storable as DVS

data EliasFano64 = EliasFano64
  { efHi     :: DVS.Vector Word64
  , efLo     :: PackedVector64
  , efLoBits :: Int
  , efCount  :: Count
  } deriving (Eq, Show)

class FromListWord64 a where
  fromList :: [Word64] -> a

class ToListWord64 a where
  toList :: a -> [Word64]

class FromEliasFano64 a where
  fromEliasFano64 :: EliasFano64 -> a

class ToEliasFano64 a where
  toEliasFano64 :: a -> EliasFano64

instance FromListWord64 EliasFano64 where
  fromList ws = case lastMay ws of
    Just end' -> EliasFano64
      { efHi      = DVS.fromList (packToWord64 (packToWord32 (packToWord16 (packToWord8 (mkHiBits loBits' ws)))))
      , efLo      = PV.fromList loBits' ws
      , efLoBits  = fromIntegral loBits'
      , efCount   = length'
      }
      where length' = length ws
            loBits' = fromIntegral (log2 (end' `div` length')) :: Count
    Nothing -> EliasFano64
      { efHi      = DVS.empty
      , efLo      = PV.empty
      , efLoBits  = 0
      , efCount   = 0
      }

instance ToListWord64 EliasFano64 where
  toList ef = gen `fmap` take (efCount ef) [0 ..]
    where gen :: Int -> Word64
          gen i = let pos             = (efLoBits ef * i)                                         in
                  let (index, offset) = pos `quotRem` 64                                          in
                  let loValue         = (efLo ef !!! fromIntegral index) .>. fromIntegral offset  in
                  let hiValue         = (efLo ef !!! fromIntegral index) .>. fromIntegral offset  in
                  loValue + hiValue

-- instance AtIndex EliasFano64 where
--   (!!!)   v i = v !! fromIntegral i
--   atIndex v i = v !! fromIntegral i
--   {-# INLINE (!!!)   #-}
--   {-# INLINE atIndex #-}
