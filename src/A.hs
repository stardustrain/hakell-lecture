module A (
  numUniques,
  groupWord,
  encode,
  decode
) where

import Data.List (nub, sort, words, group, any, tails, isPrefixOf)
import Data.Char (ord, chr)

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
