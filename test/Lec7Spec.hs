module Lec7Spec where

import Test.Hspec
import Lec7

spec :: Spec
spec = do
  describe "area" $ do
    it "return area of circle" $
      area $ Circle 10.0 20.0 10.0 `shouldBe` 314.15927
    it "return area of rectangle" $
      area $ Rect 0.0 0.0 100.0 100.0 `shouldBe` 10000.0
  describe "mapToRadius" $ do
    it "receive interger list and return Shape type list" $
      mapToRadius [1.0, 2.0, 3.0] `shouldReturn` [Circle 10 20 1, Circle 10 20 2, Circle 10 20 3]