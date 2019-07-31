{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.EliasFanoSpec (spec) where

import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.EliasFano
import HaskellWorks.Data.EliasFano.Internal
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Vector.Storable                          as DVS
import qualified HaskellWorks.Data.PackedVector.PackedVector64 as PV
import qualified Hedgehog.Gen                                  as G
import qualified Hedgehog.Range                                as R

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.EliasFanoSpec" $ do
  it "List to EliasFano" $ requireTest $ do
    ws <- forAll $ pure $ [2, 3, 5, 7, 11, 13, 24]
    let actual = fromWord64s ws
    let expected = EliasFano
          { efBucketBits  = makeCsPoppy (DVS.fromList [4443])
          , efLoSegments  = PV.fromList 2 [2, 3, 1, 3, 3, 1, 0]
          , efLoBitCount  = 2
          , efCount       = 7
          }
    actual === expected
  it "hiSegment <-> bucketBits round trip" $ requireProperty $ do
    vs <- forAll $ G.list (R.linear 1 100) (G.word64 (R.linear 0 20))
    ws <- forAll $ pure $ drop 1 $ scanl (+) 0 vs
    maxW <- forAll $ pure $ last ws
    bucketBitsToHiSegment (hiSegmentToBucketBits maxW ws) === ws
  it "List to EliasFano" $ requireTest $ do
    ws <- forAll $ pure $ EliasFano
          { efBucketBits  = makeCsPoppy (DVS.fromList [4443])
          , efLoSegments  = PV.fromList 2 [2, 3, 1, 3, 3, 1, 0]
          , efLoBitCount  = 2
          , efCount       = 7
          }
    let actual = toWord64s ws
    let expected = [2, 3, 5, 7, 11, 13, 24]
    actual === expected
  it "List to EliasFano 2" $ requireTest $ do
    let ws =  [    0,    5,    6,   14,   20,   29,   39,   51
              ,   52,   60,   64,   71,   76,   87,   97,  103
              ,  122,  135,  233,  245,  657,  662,  663,  671
              ,  676,  684,  690,  701,  709,  725,  734,  755
              ,  783,  796,  834,  841,  842,  845,  855,  861
              ,  883,  888,  910,  917,  925,  936,  945,  962
              ,  982,  998, 1019, 1028, 1029, 1040, 1044, 1048
              , 1057, 1066, 1075, 1080, 1090, 1097, 1098, 1102
              , 1117, 1121, 1136, 1141, 1142, 1148, 1157, 1167
              , 1168, 1173, 1184, 1189, 1201, 1205, 1216, 1225
              , 1226, 1231, 1244, 1249, 1250, 1253, 1257, 1262
              , 1274, 1279, 1280, 1283, 1289, 1294, 1315, 1320
              , 1321, 1324, 1330, 1335, 1346, 1351, 1352, 1355
              , 1362, 1367, 1403, 1414, 1423, 1424, 1427, 1438
              , 1446, 1447, 1452, 1477, 1482, 1483, 1486, 1493
              , 1498, 1529, 1533, 1534, 1537, 1544, 1549, 1581
              ]
    let ef = fromWord64s ws :: EliasFano
    _ <- forAll $ pure ef
    let actual = toWord64s ef
    let expected = ws
    actual === expected
  it "Round trip" $ requireProperty $ do
    vs <- forAll $ G.list (R.linear 0 100) (G.word64 (R.linear 1 20))
    ws <- forAll $ pure $ drop 1 $ scanl (+) 0 vs
    ef :: EliasFano <- forAll $ pure $ fromWord64s ws
    let actual = toWord64s ef
    actual === (ws :: [Word64])
  it "atIndex" $ requireTest $ do
    ef <- forAll $ pure $ EliasFano
          { efBucketBits  = makeCsPoppy (DVS.fromList [4443])
          , efLoSegments  = PV.fromList 2 [2, 3, 1, 3, 3, 1, 0]
          , efLoBitCount  = 2
          , efCount       = 7
          }
    let actual = fmap (atIndex ef) [0 .. end ef - 1]
    let expected = [2, 3, 5, 7, 11, 13, 24]
    actual === expected
  it "hiSegmentToWords" $ requireTest $ do
    ws <- forAll $ pure $ [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]

    filter (/= ' ') (bitShow (hiSegmentToWords ws)) === concat
      [ "01101010", "01000100", "00010000", "00001000", "00000000", "00100000", "00000000", "00000000"
      , "10000000", "00000000", "00000000", "00000000", "00010000", "00000000", "00000000", "00000000"
      , "00000000", "00000000", "00000000", "00010000", "00000000", "00000000", "00000000", "00000000"
      ]
