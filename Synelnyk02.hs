{-# OPTIONS_GHC -Wall #-}
module Synelnyk02 where

-- Task 1 -----------------------------------------
sumFl :: [Integer] -> Integer
sumFl  xs =foldl (+) 0 xs
  
-- Task  2 -----------------------------------------
productFr :: [Integer] -> Integer
productFr xs = foldr (*) 1 xs

-- Task  3 -----------------------------------------
concatFr :: [Int] -> [Int] -> [Int]
concatFr xs ys = foldr (:) ys xs

-- Task  4 -----------------------------------------
insert :: [Int] -> Int -> [Int]
insert [] v = [v]
insert (x:xs) v
    | x < v = x : insert xs v
    | otherwise = v : insert xs x

sortInsert :: [Int] -> [Int]
sortInsert [] = []
sortInsert xs = foldl insert [] xs

-- Task  5 -----------------------------------------
findIndices ::(Int -> Bool) -> [Int] -> [Int] 
findIndices  = undefined

-- Task  6 -----------------------------------------
allReverse :: [String] -> [String]
allReverse xxs = reverse (map reverse xxs)

-- Task  7  -----------------------------------------
noDigits :: String -> String
noDigits xs = filter (\x -> not (elem x ['1' .. '9'])) xs

-- Task  8 ------------------------------------------
cntGood :: [Int -> Bool] -> Int -> Int
cntGood ps v = length [x | x <- ps, x v]

-- Task  9 ------------------------------------------
trianglePas :: [[Integer]]
trianglePas = iterate (\x -> zipWith (+) ([0] ++ x) (x ++ [0])) [1]

-- Task  10 -----------------------------------------
factorialWithZero :: [Integer]
factorialWithZero = 1 : zipWith (*) factorialWithZero [1..]
factorialsM :: [Integer]
factorialsM = tail (factorialWithZero)

