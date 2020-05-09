{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.EliasFano.ReferenceSpec (spec) where

import Data.Word
import HaskellWorks.Data.EliasFano.Reference
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

{- HLINT ignore "Redundant do"        -}

spec :: Spec
spec = describe "HaskellWorks.Data.EliasFano.ReferenceSpec" $ do
  it "List to EliasFano" $ require $ withTests 1 $ property $ do
    ws <- forAll $ pure $ [2, 3, 5, 7, 11, 13, 24]
    let actual = fromWord64s ws
    let expected = EliasFano
          { efBucketBits  = [ True
                            , True
                            , False
                            , True
                            , True
                            , False
                            , True
                            , False
                            , True
                            , False
                            , False
                            , False
                            , True
                            , False
                            ]
          , efLoSegments  = [2, 3, 1, 3, 3, 1, 0]
          , efLoBitCount  = 2
          , efCount       = 7
          }
    actual === expected
  it "hiSegment <-> bucketBits round trip" $ require $ property $ do
    vs <- forAll $ G.list (R.linear 1 100) (G.word64 (R.linear 0 20))
    ws <- forAll $ pure $ drop 1 $ scanl (+) 0 vs
    maxW <- forAll $ pure $ last ws
    bucketBitsToHiSegment (hiSegmentToBucketBits maxW ws) === ws
  it "List to EliasFano" $ require $ withTests 1 $ property $ do
    ws <- forAll $ pure $ EliasFano
          { efBucketBits  = [ True
                            , True
                            , False
                            , True
                            , True
                            , False
                            , True
                            , False
                            , True
                            , False
                            , False
                            , False
                            , True
                            , False
                            ]
          , efLoSegments  = [2, 3, 1, 3, 3, 1, 0]
          , efLoBitCount  = 2
          , efCount       = 7
          }
    let actual = toWord64s ws
    let expected = [2, 3, 5, 7, 11, 13, 24]
    actual === expected
  it "Round trip" $ require $ property $ do
    vs <- forAll $ G.list (R.linear 0 100) (G.word64 (R.linear 1 20))
    ws <- forAll $ pure $ drop 1 $ scanl (+) 0xffffffffffffffff vs
    ef :: EliasFano <- forAll $ pure $ fromWord64s ws
    toWord64s ef === (ws :: [Word64])
