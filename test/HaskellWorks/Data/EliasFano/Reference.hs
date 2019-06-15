{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.EliasFano.Reference
  ( EliasFano(..)
  , divup
  , fromListWord64
  , toListWord64
  , hiSegmentToBucketBits
  , bucketBitsToHiSegment
  ) where

import Data.Bits                      (countLeadingZeros, finiteBitSize)
import Data.Int
import Data.Word
import HaskellWorks.Data.AtIndex      hiding (end)
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.Log2
import HaskellWorks.Data.Foldable
import HaskellWorks.Data.Positioning
import Prelude                        hiding (length, take)

data EliasFano = EliasFano
  { efBucketBits :: [Bool]   -- 1 marks bucket, 0 marks skip to next
  , efLoSegments :: [Word64] -- Lower segment of each entry
  , efLoBitCount :: Count    -- Number of bits in each lower segment
  , efCount      :: Count    -- Number of entries
  } deriving (Eq, Show)

-- | Calculates ceil (n / d) for small numbers
divup :: Word64 -> Word64 -> Word64
divup n d = fromIntegral (-((-sn) `div` sd)) :: Word64
  where sd = fromIntegral d :: Int64
        sn = fromIntegral n :: Int64

hiSegmentToBucketBits :: Word64 -> [Word64] -> [Bool]
hiSegmentToBucketBits lastWord = go 0
  where go :: Word64 -> [Word64] -> [Bool]
        go i []     | i >= lastWord = []
        go i (a:as) | i == a        = True:go i as
        go i (a:as) | i <  a        = False:go (i + 1) (a:as)
        go i []     = False:go (i + 1) []
        go _ (_:_)  = error "Invalid entry"

bucketBitsToHiSegment :: [Bool] -> [Word64]
bucketBitsToHiSegment = go 0
  where go :: Word64 -> [Bool] -> [Word64]
        go _ []          = []
        go i (True:bs)   = i:go  i      bs
        go i (False: bs) =   go (i + 1) bs

fromListWord64 :: [Word64] -> EliasFano
fromListWord64 ws = case foldLast ws of
  Just end' -> EliasFano
    { efBucketBits  = hiSegmentToBucketBits (bucketEnd - 1) his
    , efLoSegments  = los
    , efLoBitCount  = loBits'
    , efCount       = length'
    }
    where length'   = length ws
          loBits'   = fromIntegral (log2 ((end' + 2) `divup` length')) :: Count
          hiMask    = maxBound .<. loBits' :: Word64
          loMask    = comp hiMask :: Word64
          his       = (.>. loBits') . (.&. hiMask) <$> ws
          los       = (.&. loMask) <$> ws
          hiEnd     = end' .>. loBits'
          bucketEnd = 1 .<. fromIntegral (finiteBitSize hiEnd - countLeadingZeros hiEnd) :: Word64
  Nothing -> EliasFano
    { efBucketBits  = []
    , efLoSegments  = []
    , efLoBitCount  = 0
    , efCount       = 0
    }

toListWord64 :: EliasFano -> [Word64]
toListWord64 ef = uncurry combine <$> zip (bucketBitsToHiSegment (efBucketBits ef)) (efLoSegments ef)
  where combine hi lo = (hi .<. efLoBitCount ef) .|. lo
