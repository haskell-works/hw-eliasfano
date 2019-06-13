{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module HaskellWorks.Data.EliasFano
  ( EliasFano(..)
  , FromListWord64(..)
  , ToListWord64(..)
  , size
  ) where

import Control.DeepSeq
import Data.Bits                                 (countLeadingZeros, finiteBitSize)
import Data.Word
import GHC.Generics
import HaskellWorks.Data.AtIndex                 hiding (end)
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.Log2
import HaskellWorks.Data.EliasFano.Internal
import HaskellWorks.Data.Foldable
import HaskellWorks.Data.FromListWord64
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.ToListWord64
import Prelude                                   hiding (length, take)

import qualified Data.Vector.Storable                          as DVS
import qualified HaskellWorks.Data.PackedVector.PackedVector64 as PV

data EliasFano = EliasFano
  { efBucketBits :: !(DVS.Vector Word64)  -- 1 marks bucket, 0 marks skip to next
  , efLoSegments :: !PV.PackedVector64    -- Lower segment of each entry
  , efLoBitCount :: !Count                -- Number of bits in each lower segment
  , efCount      :: !Count                -- Number of entries
  } deriving (Eq, Show, Generic)

instance NFData EliasFano

size :: EliasFano -> Count
size = efCount

instance FromListWord64 EliasFano where
  fromListWord64 ws = case foldLast ws of
    Just end' -> EliasFano
      { efBucketBits  = hiSegmentToBucketWords (bucketEnd - 1) his
      , efLoSegments  = PV.fromList loBits' los
      , efLoBitCount  = loBits'
      , efCount       = length'
      }
      where length'   = length ws
            loBits'   = fromIntegral (log2 (end' `divup` length')) :: Count
            hiMask    = maxBound .<. loBits' :: Word64
            loMask    = comp hiMask :: Word64
            his       = (.>. loBits') . (.&. hiMask) <$> ws
            los       = (.&. loMask) <$> ws
            hiEnd     = end' .>. loBits'
            bucketEnd = 1 .<. fromIntegral (finiteBitSize hiEnd - countLeadingZeros hiEnd) :: Word64
    Nothing -> EliasFano
      { efBucketBits  = DVS.empty
      , efLoSegments  = PV.empty
      , efLoBitCount  = 0
      , efCount       = 0
      }

instance ToListWord64 EliasFano where
  toListWord64 ef = uncurry combine <$> zip (bucketBitsToHiSegment bucketBits) (PV.toList (efLoSegments ef))
    where combine hi lo = (hi .<. efLoBitCount ef) .|. lo
          bucketBits = bucketWordsToBucketBools (efCount ef) (efBucketBits ef)

instance Container EliasFano where
  type Elem EliasFano = Word64

instance Length EliasFano where
  length = efCount
  {-# INLINE length #-}

instance AtIndex EliasFano where
  (!!!)         = atIndex
  atIndex ef i  = atIndex (efLoSegments ef) i .|. ((select1 (efBucketBits ef) j - j) .<. efLoBitCount ef)
    where j = fromIntegral i + 1
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}
