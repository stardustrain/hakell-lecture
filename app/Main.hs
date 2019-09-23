module Main where

import Control.Monad (forever)
import Data.Char (toUpper)
import Lec9 (shortLinesOnly)

main :: IO ()
-- main = do
--   line <- getLine
--   if null line
--     then return ()
--     else do
--       print $ reverseWords line
--       main
main = do
  contents <- getContents
  putStrLn $ shortLinesOnly contents