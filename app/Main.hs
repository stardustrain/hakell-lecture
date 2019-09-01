module Main where

import Lib
import Lec6

main :: IO ()
main = print (findByKey "aa" [("aa", "bb"), ("cc", "dd")])