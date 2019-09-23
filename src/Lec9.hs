module Lec9 (
  shortLinesOnly
) where

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 10) . lines
