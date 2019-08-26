import Test.Hspec
-- import Test.QuickCheck
import Control.Exception (evaluate)
import Lib
import A

main :: IO ()
main = hspec $ do
  it "returns the first element of a list" $ do
    doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]
    doubleEveryOther [1, 2, 3] `shouldBe` [1,4,3]

  it "encode text message" $ do
    encode 3 "hey mark" `shouldBe` "kh|#pdun"
  it "decode text message" $ do
    decode 3 "kh|#pdun" `shouldBe` "hey mark"
