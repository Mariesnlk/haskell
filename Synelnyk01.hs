{-# OPTIONS_GHC -Wall #-}
module Synelnyk01 where

-- Task 1 -----------------------------------------
power3 :: [Integer]
power3 = [x^(3::Integer) | x <- [1 ..]]

-- Task 2 -----------------------------------------
toPower3 :: [Integer]
toPower3 = [3^x | x <- [1 ..]::[Integer]]

-- Task 3 -----------------------------------------
sumPower3 :: Integer -> Integer
sumPower3 n = sum [3^x | x <- [1 .. n]]

-- Task 4 -----------------------------------------
sumPower :: Integer -> Integer -> Integer
sumPower m n = sum[m^x | x <- [1 .. n], m>=0]

-- Task 5 -----------------------------------------
quantityOfLess :: Int -> [Int] -> Int
quantityOfLess _ [] = 0
quantityOfLess n ns = if n > (head ns) then 1 + quantityOfLess n (tail ns) else  quantityOfLess n (tail ns)
lessMe :: [Int] -> [Int]
lessMe xs = [quantityOfLess x xs | x <- xs] 
 
-- Task 6 -----------------------------------------
quantityOfEqual :: Int -> [Int] -> Int
quantityOfEqual _ [] = 0
quantityOfEqual n ns = if n == (head ns) then 1 + quantityOfEqual n (tail ns) else quantityOfEqual n (tail ns)
frequency :: [Int] -> [(Int,Int)]
frequency = undefined

-- Task 7 -----------------------------------------
hailstone :: Int -> Int
hailstone n = if (mod n 2) == 0 then n `div` 2 else n*3 + 1

-- Task 8 -----------------------------------------
hailSeq :: Int -> [Int]
hailSeq n = if (n == 1) then [1] else [n]++hailSeq(hailstone n)

-- Task 9 -----------------------------------------
allHailSeq :: [[Int]]
allHailSeq = [hailSeq x | x <- [1 ..]] 

-- Task 10 -----------------------------------------
firstHailSeq :: Int -> Int
firstHailSeq l = head [x | x <- [1 ..], (length (hailSeq x) == l)]

