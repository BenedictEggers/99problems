-- Ben Eggers <ben.eggers36@gmail.com>
-- Problems 31-41

module Arithmetic where

-- Problem 31
isPrime :: Int -> Bool
isPrime n = null [x | x <- [2..floor $ sqrt $ fromIntegral n], n `mod` x == 0]

-- Problem 32
gcd' :: Int -> Int -> Int
gcd' n m = if n == m then n else gcd (max n m `mod` min n m) (min n m)

-- Problem 33
coprime :: Int -> Int -> Bool
coprime n m = gcd n m == 1

-- Problem 34
totient :: Int -> Int
totient n = length [x | x <- [1..n], coprime x n]

-- Problem 35
primeFactors :: Int -> [Int]
primeFactors n = helper n []
    where helper 1 acc = reverse acc
          helper n acc = let x = head [q | q <- [2..n], isPrime q && n `mod` q == 0]
                         in helper (quot n x) (x : acc)

-- Problem 36
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult n = helper (primeFactors n) []
    where helper [] acc = reverse acc
          helper (x:xs) acc = helper
                            (dropWhile (==x) xs)
                            ((x, (length (takeWhile (==x) xs) + 1)):acc)

-- Problem 37
totientImproved :: Int -> Int
totientImproved n  = helper (primeFactorsMult n) 1
    where helper [] acc = acc
          helper ((f, m):facs) acc = helper facs (acc * (f - 1) * f ^ (m - 1))