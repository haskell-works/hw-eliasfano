{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module HaskellWorks.Data.EliasFanoSpec (spec) where

import Data.Word
import HaskellWorks.Data.EliasFano
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Vector.Storable                          as DVS
import qualified HaskellWorks.Data.PackedVector.PackedVector64 as PV
import qualified Hedgehog.Gen                                  as G
import qualified Hedgehog.Range                                as R

import Debug.Trace

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.EliasFanoSpec" $ do
  xit "List to EliasFano" $ require $ withTests 1 $ property $ do
    ws <- forAll $ pure $ [2, 3, 5, 7, 11, 13, 24]
    let actual = fromListWord64 ws
    let expected = EliasFano
          { efBucketBits  = DVS.fromList [4443]
          , efLoSegments  = PV.fromList 2 [2, 3, 1, 3, 3, 1, 0]
          , efLoBitCount  = 2
          , efCount       = 7
          }
    actual === expected
  xit "hiSegment <-> bucketBits round trip" $ require $ property $ do
    vs <- forAll $ G.list (R.linear 1 100) (G.word64 (R.linear 0 20))
    ws <- forAll $ pure $ drop 1 $ scanl (+) 0 vs
    maxW <- forAll $ pure $ last ws
    bucketBitsToHiSegment (hiSegmentToBucketBits maxW ws) === ws
  xit "List to EliasFano" $ require $ withTests 1 $ property $ do
    ws <- forAll $ pure $ EliasFano
          { efBucketBits  = DVS.fromList [4443]
          , efLoSegments  = PV.fromList 2 [2, 3, 1, 3, 3, 1, 0]
          , efLoBitCount  = 2
          , efCount       = 7
          }
    let actual = toListWord64 ws
    let expected = [2, 3, 5, 7, 11, 13, 24]
    actual === expected
  it "Round trip" $ require $ property $ do
    vs <- forAll $ G.list (R.linear 0 100) (G.word64 (R.linear 1 20))
    let !_ = trace ("vs: " ++ show vs) ()
    ws <- forAll $ pure $ drop 1 $ scanl (+) 0 vs
    let !_ = trace ("ws: " ++ show ws) ()
    ef :: EliasFano <- forAll $ pure $ fromListWord64 ws
    let !_ = trace ("ef: " ++ show ef) ()
    let actual = toListWord64 ef
    let !_ = trace ("actual: " ++ show actual) ()
    actual === (ws :: [Word64])
