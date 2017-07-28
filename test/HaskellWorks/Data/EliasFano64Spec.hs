{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.EliasFano64Spec (spec) where

import Data.Word
import HaskellWorks.Data.EliasFano64
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.EliasFano64Spec" $ do
  it "Empty" $ require $ property $ do
    vs <- forAll $ G.list (R.linear 0 100) (G.word64 (R.linear 1 20))
    ws <- forAll $ pure $ drop 1 $ scanl (+) (-1) vs
    ef :: EliasFano64 <- forAll $ pure $ fromList ws
    toList ef === (ws :: [Word64])
