module Lec7Spec where

import Test.Hspec
import Lec7

spec :: Spec
spec = do
  describe "area" $ do
    it "return area of circle" $
      area (Circle (Point 10 20) 10) `shouldBe` 314.15927
    it "return area of rectangle" $
      area (Rect (Point 0 0) (Point 100 100)) `shouldBe` 10000.0
  describe "mapToRadius" $
    it "receive interger list and return Shape type list" $
      mapToRadius [1.0, 2.0, 3.0] `shouldBe` [Circle (Point 10 20) 1, Circle (Point 10 20) 2, Circle (Point 10 20) 3]