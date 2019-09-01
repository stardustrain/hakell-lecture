module ASpec where

import Test.Hspec
import A

spec :: Spec
spec = do
  describe "test" $ do
    it "encode text message" $ do
      encode 3 "hey mark" `shouldBe` "kh|#pdun"
    it "decode text message" $ do
      decode 3 "kh|#pdun" `shouldBe` "hey mark"