{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.EliasFano64Spec (spec) where

import           Data.Word
import           HaskellWorks.Data.EliasFano64
import           Test.Hspec

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.EliasFano64Spec" $ do
  it "Empty" $
    fromEliasFano64 (toEliasFano64 ([] :: [Word64])) `shouldBe` ([] :: [Word64])
