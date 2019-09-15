module Lec71Spec where

import Test.Hspec
import Lec71

spec :: Spec
spec = do
  describe "singletonTree" $
    it "return exist one of node tree" $
      singletonTree 5 `shouldBe` Node 5 EmptyTree EmptyTree
  describe "insertTree" $ do
    it "return singleton tree when passed empty tree" $
      insertTree 5 EmptyTree `shouldBe` Node 5 EmptyTree EmptyTree
    it "return tree attached left side when passed value lt parent node" $ do
      insertTree 3 (Node 5 EmptyTree EmptyTree) `shouldBe` Node 5 (Node 3 EmptyTree EmptyTree) EmptyTree
      insertTree 1 (Node 5 (Node 3 EmptyTree EmptyTree) (Node 7 EmptyTree EmptyTree)) `shouldBe` Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) EmptyTree) (Node 7 EmptyTree EmptyTree)
    it "return tree attached right side when passed value gt parent node" $
      insertTree 7 (Node 5 (Node 3 EmptyTree EmptyTree) EmptyTree) `shouldBe` Node 5 (Node 3 EmptyTree EmptyTree) (Node 7 EmptyTree EmptyTree)
  describe "treeElem" $ do
    it "return True when exist this value" $ do
      treeElem 5 (Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) EmptyTree) (Node 7 EmptyTree EmptyTree)) `shouldBe` True
      treeElem 1 (Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) EmptyTree) (Node 7 EmptyTree EmptyTree)) `shouldBe` True
    it "return False when not exist value or received EmptyTree" $ do
      treeElem 9 (Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) EmptyTree) (Node 7 EmptyTree EmptyTree)) `shouldBe` False
      treeElem 5 EmptyTree `shouldBe` False
  describe "YesNo" $
    it "return True or False" $ do
      yesNo "" `shouldBe` False
      yesNo (0 :: Int) `shouldBe` False
      yesNo [] `shouldBe` False
      yesNo "test" `shouldBe` True
      yesNo ["test"] `shouldBe` True
      yesNo (1 :: Int) `shouldBe` True
