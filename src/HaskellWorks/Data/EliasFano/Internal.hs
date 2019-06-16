{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module HaskellWorks.Data.EliasFano.Internal
  ( divup
  , hiSegmentToBucketBits
  , bucketBitsToHiSegment
  , bucketBoolsToBucketWords
  , bucketWordsToBucketBools
  , hiSegmentToWords
  , foldCountAndLast
  ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning
import Prelude                        hiding (length, take)

import qualified Data.Vector.Storable as DVS
import qualified Prelude              as P

foldCountAndLast :: Foldable t => t a -> (Maybe a, Count)
foldCountAndLast = foldl go (Nothing, 0)
  where go :: (Maybe a, Count) -> a -> (Maybe a, Count)
        go (_, n) a = (Just a, n + 1)
{-# INLINE foldCountAndLast #-}

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

hiSegmentToWords :: [Word64] -> [Word64]
hiSegmentToWords = go 0 0 0
  where go :: Count -> Word64 -> Word64 -> [Word64] -> [Word64]
        go n acc lst us@(v:vs) = if n < 64
          then if lst < v
            then go (n + 1)  acc                (lst + 1) us
            else go (n + 1) (acc .|. (1 .<. n))  v        vs
          else acc:go 0 0 lst us
        go 0 _   _    _         = []
        go _ acc _    _         = [acc]

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
