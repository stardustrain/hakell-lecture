module Lib
    ( someFunc,
      toDigits,
      doubleEveryOther,
      sumDigits
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

doubleInList :: [Int] -> [Int]
doubleInList [] = []
doubleInList [x] = [x]
doubleInList (x:n:ns) = x:(n * 2) : (doubleInList ns)

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther xs = reverse (doubleInList (reverse xs))

sumDigits :: [Int] -> Int
sumDigits xs = sum (toDigits (read (join xs) :: Int))

-- validate :: Int -> Bool

