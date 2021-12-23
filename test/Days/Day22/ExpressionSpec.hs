module Days.Day22.ExpressionSpec where

import Days.Day22.Box3 (mkBox)
import Days.Day22.Expression
import Test.Hspec
import Prelude

spec =
  describe "Expression" $ do
    describe "volume" $ do
      describe "base case" $ do
        it "defers calculation to the box calculator" $ do
          volume (Base (mkBox (0, 0, 0) (2, 2, 2))) `shouldBe` 8
    describe "union" $ do
      it "calculates union properly" $ do
        let b1 = mkBox (0, 0, 0) (2, 2, 2)
        let b2 = mkBox (1, 1, 1) (3, 3, 3)
        volume (Union (Base b1) (Base b2)) `shouldBe` 15
        let b3 = mkBox (2, 2, 2) (4, 4, 4)
        volume (Union (Union (Base b1) (Base b2)) (Base b3)) `shouldBe` 22
    describe "difference" $ do
      it "calculates difference properly" $ do
        let b1 = mkBox (0, 0, 0) (2, 2, 2)
        let b2 = mkBox (1, 1, 1) (3, 3, 3)
        volume (Difference (Base b1) (Base b2)) `shouldBe` 7
    describe "all ops" $ do
      it "calculates properly" $ do
        let b1 = mkBox (0, 0, 0) (6, 6, 6)
        let b2 = mkBox (3, 3, 3) (9, 9, 9)
        let b3 = mkBox (3, 3, 3) (6, 6, 6)
        let b4 = mkBox (5, 5, 5) (6, 6, 6)
        volume (Union (Difference (Union (Base b1) (Base b2)) (Base b3)) (Base b4)) `shouldBe` 379
    describe "normalForm" $ do
      it "simplifies intersections" $ do
        let b1 = mkBox (0, 0, 0) (1, 1, 1)
        let b2 = mkBox (1, 1, 1) (2, 2, 2)
        let b3 = mkBox (2, 2, 2) (3, 3, 3)
        normalForm (Intersection (Intersection (Base b1) (Base b2)) (Base b3)) `shouldBe` Intersection (Base $ mkBox (0, 0, 0) (0, 0, 0)) (Base b3)