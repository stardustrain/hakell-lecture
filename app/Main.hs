module Main where

import Data.Char (toUpper)
import Lib (reverseWords)

main :: IO ()
main = do
  line <- getLine
  if null line
    then return ()
    else do
      print $ reverseWords line
      main
