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
  describe "nudge" $ do
    it "return Rect" $
      nudge (baseRect 10 20) 20 30 `shouldBe` Rect (Point 20 30) (Point 30 50)
    it "return Circle" $
      nudge (Circle (Point 10 10) 20) 20 30 `shouldBe` Circle (Point 30 40) 20
  describe "Person" $
    it "make person type variable and return first name" $
      let guy = Person { firstName="test"
                        ,lastName="test2"
                        ,age=25
                        ,height=184.5
                        ,phoneNumber="555-5555"
                      }
      in firstName guy `shouldBe` "test"
  describe "tellPerson" $
    it "return description for person" $
      let guy = Person { firstName="J"
                        ,lastName="Kim"
                        ,age=25
                        ,height=184.5
                        ,phoneNumber="555-5555"
                      }
      in tellPerson guy `shouldBe` "J Kim has 25 years old."
  describe "Day" $ do
    it "return min or max day" $ do
      (minBound :: Day) `shouldBe` Mon
      (maxBound :: Day) `shouldBe` Sun
    it "return next or previous day" $ do
      succ Mon `shouldBe` Tue
      pred Sat `shouldBe` Fri
    it "return array from Day enum" $
      ([minBound .. maxBound] :: [Day]) `shouldBe` [Mon, Tue, Wed, Thur, Fri, Sat, Sun]
