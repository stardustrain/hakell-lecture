module Lec71 (
  Tree(..),
  YesNo(yesNo),
  singletonTree,
  insertTree,
  treeElem
) where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Eq)

singletonTree :: (Ord a) => a -> Tree a
singletonTree a = Node a EmptyTree EmptyTree

insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree x EmptyTree = singletonTree x
insertTree x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (insertTree x left) right
  | x > a = Node a left (insertTree x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

class YesNo a where
  yesNo :: a -> Bool

instance YesNo Int where
  yesNo 0 = False
  yesNo _ = True

instance YesNo [a] where
  yesNo [] = False
  yesNo _ = True

instance YesNo Bool where
  yesNo False = False
  yesNo True = True
