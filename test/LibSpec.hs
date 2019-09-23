module LibSpec where

import Test.Hspec
import Lib

spec :: Spec
spec = describe "test" $ do
    it "returns the first element of a list" $ do
      doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]
      doubleEveryOther [1, 2, 3] `shouldBe` [1,4,3]
    describe "reverseWords" $
      it "return reversed words for passed from parameters" $
        reverseWords "Clean up on aisle number line" `shouldBe` "naelC pu no elsia rebmun enil"
