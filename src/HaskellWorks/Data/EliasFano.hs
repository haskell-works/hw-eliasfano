{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module HaskellWorks.Data.EliasFano
  ( EliasFano(..)
  , fromWord64s
  , fromWord64sWithLastAndCount
  , toWord64s
  , empty
  , size
  ) where

import Control.DeepSeq
import Data.Word
import GHC.Generics
import HaskellWorks.Data.AtIndex                 hiding (end)
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.Log2
import HaskellWorks.Data.EliasFano.Internal
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Select1
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

empty :: EliasFano
empty = EliasFano
  { efBucketBits  = DVS.empty
  , efLoSegments  = PV.empty
  , efLoBitCount  = 0
  , efCount       = 0
  }

fromWord64sWithLastAndCount :: Word64 -> Count -> [Word64] -> EliasFano
fromWord64sWithLastAndCount lst count ws = EliasFano
  { efBucketBits  = DVS.fromList $ hiSegmentToWords his
  , efLoSegments  = PV.fromListN count loBits' los
  , efLoBitCount  = loBits'
  , efCount       = count
  }
  where loBits'   = fromIntegral (log2 (lst `divup` fromIntegral count)) :: Count
        hiMask    = maxBound .<. loBits' :: Word64
        loMask    = comp hiMask :: Word64
        his       = (.>. loBits') . (.&. hiMask) <$> ws
        los       = (.&. loMask) <$> ws

fromWord64s :: [Word64] -> EliasFano
fromWord64s ws = case foldCountAndLast ws of
  (Just lst, count) -> fromWord64sWithLastAndCount lst count ws
  (Nothing, _)      -> empty

toWord64s :: EliasFano -> [Word64]
toWord64s ef = uncurry combine <$> zip (bucketBitsToHiSegment bucketBits) (PV.toList (efLoSegments ef))
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
