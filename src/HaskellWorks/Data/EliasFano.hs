{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.EliasFano
  ( EliasFano(..)
  , FromListWord64(..)
  , ToListWord64(..)
  , divup
  , hiSegmentToBucketBits
  , bucketBitsToHiSegment
  ) where

import Control.DeepSeq
import Data.Bits                            (countLeadingZeros, finiteBitSize)
import Data.Int
import Data.Word
import GHC.Generics
import HaskellWorks.Data.AtIndex            hiding (end)
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.Log2
import HaskellWorks.Data.EliasFano.Internal
import HaskellWorks.Data.FromListWord64
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.ToListWord64
import Prelude                              hiding (length, take)

import qualified Data.Vector.Storable                          as DVS
import qualified HaskellWorks.Data.PackedVector.PackedVector64 as PV
import qualified Prelude                                       as P

data EliasFano = EliasFano
  { efBucketBits :: !(DVS.Vector Word64)  -- 1 marks bucket, 0 marks skip to next
  , efLoSegments :: !PV.PackedVector64    -- Lower segment of each entry
  , efLoBitCount :: !Count                -- Number of bits in each lower segment
  , efCount      :: !Count                -- Number of entries
  } deriving (Eq, Show, Generic)

instance NFData EliasFano

-- | Calculates ceil (n / d) for small numbers
divup :: Word64 -> Word64 -> Word64
divup n d = fromIntegral (-((-sn) `div` sd)) :: Word64
  where sd = fromIntegral d :: Int64
        sn = fromIntegral n :: Int64

bucketBoolsToBucketWords :: [Bool] -> DVS.Vector Word64
bucketBoolsToBucketWords bs = DVS.unfoldrN ((P.length bs `div` 64) + 1) gen bs
  where gen :: [Bool] -> Maybe (Word64, [Bool])
        gen cs = if not (null cs) then genWord cs 0 0 else Nothing
        genWord :: [Bool] -> Count -> Word64 -> Maybe (Word64, [Bool])
        genWord (True :cs) i acc | i < 64 = genWord cs (i + 1) (acc .|. (1 .<. i))
        genWord (False:cs) i acc | i < 64 = genWord cs (i + 1)  acc
        genWord        cs  _ acc = Just (acc, cs)

bucketWordsToBucketBools :: Count -> DVS.Vector Word64 -> [Bool]
bucketWordsToBucketBools n v = fst (DVS.foldl go (id, n) v) []
  where go :: ([Bool] -> [Bool], Count) -> Word64 -> ([Bool] -> [Bool], Count)
        go (bs, c) w | c > 0 = case goWord c 64 w of
                                (cs, finalCount) -> (bs . cs, finalCount)
        go (bs, _) _         = (bs, 0)
        goWord :: Count -> Count -> Word64 -> ([Bool] -> [Bool], Count)
        goWord c i w | c > 0 && i > 0 = let b = (w .&. 1) /= 0
                                        in case goWord (if b then c - 1 else c) (i - 1) (w .>. 1) of
                                              (bs, finalCount) -> ((b:) . bs, finalCount)
        goWord c _ _                  = (id, c)

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

instance FromListWord64 EliasFano where
  fromListWord64 ws = case lastMaybe ws of
    Just end' -> EliasFano
      { efBucketBits  = bucketBoolsToBucketWords (hiSegmentToBucketBits (bucketEnd - 1) his)
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

-- instance AtIndex EliasFano where
--   (!!!)   v i = v !! fromIntegral i
--   atIndex v i = v !! fromIntegral i
--   {-# INLINE (!!!)   #-}
--   {-# INLINE atIndex #-}
