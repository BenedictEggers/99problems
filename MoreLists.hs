-- Ben Eggers <ben.eggers36@gmail.com>
-- Problems 11-20

module MoreLists where

data Encoded a = Single a | Multiple Int a deriving (Show, Eq)

-- Problem 11
encodeModified xs = []

-- Proble 12
decodeModified xs = []

-- Problem 13
encodeDirect xs = []

-- Problem 14
dupli [] = []
dupli (x:xs) = x : x : (dupli xs)

-- Problem 15
repli xs n= []

-- Problem 16
dropEvery xs n = []

-- Problem 17
split (x:xs) n = ([x], xs)

-- Problem 18
slice list start end = []

-- Problem 19
rotate xs n = []

-- Problem 20
removeAt n (x:xs) = (x, xs)