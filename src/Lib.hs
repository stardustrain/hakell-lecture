module Lib
    ( someFunc,
      toDigits,
      doubleEveryOther,
      sumDigits,
      validate
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

toDigits :: Int -> [Int]
toDigits 0 = []
toDigits n = if n < 0
                then []
                else map (\x -> read[x] :: Int) (show n)

toDigitsRev n = reverse (toDigits n)

join :: [Int] -> String
join xs = foldr (\a b -> (show a) ++ b) "" xs

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:n:ns) = x:(n * 2) : (doubleEveryOther ns)

sumDigits :: [Int] -> Int
sumDigits xs = sum (toDigits (read (join xs) :: Int))

validate :: Int -> Bool

