module Main where

import Lib
import Lec6
import Lec7

main :: IO ()
main = print (nudge (baseRect 10 20) 20 30)
