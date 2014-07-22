-- Ben Eggers <ben.eggers36@gmail.com>
-- Problems 31-41

module Arithmetic where

-- Problem 31
isPrime :: Int -> Bool
isPrime n = null [x | x <- [2..floor $ sqrt $ fromIntegral n], n `mod` x == 0]

-- Problem 32
gcd' :: Int -> Int -> Int
gcd' n m = if n == m then n else gcd (max n m `mod` min n m) (min n m)