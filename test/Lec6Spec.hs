module Lec6Spec where

import Test.Hspec
import Data.Map (fromList)
import Lec6

spec :: Spec
spec = do
  describe "Caesar cipher" $ do
    it "encode text message" $ 
      encode 3 "hey mark" `shouldBe` "kh|#pdun"
    it "decode text message" $
      decode 3 "kh|#pdun" `shouldBe` "hey mark"
  describe "Find number that sum of whole each number is 40" $
    it "digitSumFind" $
      digitSumFind 40 `shouldBe` Just 49999
  describe "Return value associate key" $ do
    it "findByKey return value if exist valid key" $ do
      findByKey "aa" [("aa", "bb"), ("cc", "dd")] `shouldBe` Just "bb"
      findByKey "cc" [("aa", "bb"), ("cc", "dd")] `shouldBe` Just "dd"
    it "findByKey return Nothing if dose not valid key" $ 
      findByKey "ee" [("aa", "bb"), ("cc", "dd")] `shouldBe` Nothing
  describe "Insert value with key" $ do
    it "insertKeyByValue has insert value with key and return new Map" $
      insertKeyByValue "ee" "ff" [("aa", "bb"), ("cc", "dd")] `shouldBe` fromList [("aa", "bb"), ("cc", "dd"), ("ee", "ff")]
    it "insertKeyByValue has overwrite value when exist same key and return new Map" $
      insertKeyByValue "cc" "ff" [("aa", "bb"), ("cc", "dd")] `shouldBe` fromList [("aa", "bb"), ("cc", "ff")]
    it "insertKeyByValue has return new Map" $
      let phoneBook = [("aa", "bb"), ("cc", "dd")]
        in fromList phoneBook == insertKeyByValue "bb" "ff" phoneBook `shouldBe` False
  describe "mapToDuplicateKey" $
    it "mapToDuplicateKey has merging value with same key" $
      mapToDuplicateKey [("aa", "bb"), ("cc", "dd"), ("aa", "ff")]
        `shouldBe` fromList [("aa", ["ff", "bb"]), ("cc", ["dd"])]
  describe "doubleArea" $
    it "doubleArea has return double size" $
    doubleArea 4.0 `shouldBe` 402.12387
