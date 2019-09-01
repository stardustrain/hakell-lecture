module Lec6 (
  numUniques,
  groupWord,
  encode,
  decode,
  digitSumFind,
  findByKey,
  insertKeyByValue,
  mapToDuplicateKey,
  doubleArea
) where

import Data.List (nub, sort, words, group, any, tails, isPrefixOf, find)
import Data.Char (ord, chr, digitToInt)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cube as Cube

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

groupWord :: String -> [(String, Int)]
groupWord = map (\ws -> (head ws, length ws)) . group . sort . words

isIndexOf :: (Eq a) => [a] -> [a] -> Bool
isIndexOf xs ys = any (xs `isPrefixOf`) (tails ys)

encode :: Int -> String -> String
encode offset = map (\x -> chr $ ord x + offset)

decode :: Int -> String -> String
-- decode offset = map (\x -> chr $ ord x - offset)
decode offset = encode $ negate offset

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

digitSumFind :: Int -> Maybe Int
digitSumFind x = find (\v -> digitSum v == x) [1..]

phoneBook =
  [("aa", "1111-1111")
  , ("bb", "2222-2222")
  , ("cc", "3333-3333")
  ]

findByKey :: (Ord k) => k -> [(k, v)] -> Maybe v
-- findByKey key = snd . head . filter (\(k, v) -> key == k)

-- findByKey _ [] = Nothing
-- findBykey key ((k, v):xs)
--   | key == k = Just v
--   | otherwise = findByKey key xs

-- findByKey key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing

findByKey key xs = Map.lookup key $ Map.fromList xs

insertKeyByValue :: (Ord k) => k -> a -> [(k, a)] -> Map.Map k a
insertKeyByValue k v ms = Map.insert k v $ Map.fromList ms

mapToDuplicateKey :: (Ord k) => [(k, a)] -> Map.Map k [a]
mapToDuplicateKey xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs

doubleArea :: Float -> Float
doubleArea rad = (*2) $ Sphere.area rad
