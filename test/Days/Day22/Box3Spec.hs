module Days.Day22.Box3Spec where

import Days.Common.Vec3
import Days.Day22.Box3
import Test.Hspec
import Prelude

spec = do
  describe "volume" $ do
    it "calculates the volume" $ do
      volume (mkBox (0, 0, 0) (2, -2, 2)) `shouldBe` 8
  describe "intersect" $ do
    it "calculates the box that is common to both boxes" $ do
      let box1 = mkBox (0, 0, 0) (2, 2, 2)
      let box2 = mkBox (1, 1, 1) (3, 3, 3)
      let zeroBox = mkBox (0, 0, 0) (0, 0, 0)
      (box1 `intersect` box2) `shouldBe` mkBox (1, 1, 1) (2, 2, 2)
      (box1 `intersect` zeroBox) `shouldBe` zeroBox
      (box1 `intersect` mkBox (2, 2, 2) (3, 3, 3)) `shouldBe` zeroBox