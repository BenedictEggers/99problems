-- Ben Eggers <ben.eggers36@gmail.com>
-- Problems 11-20

module MoreLists where

import MyLists( encode' )

data Encoded a = Single a | Multiple Int a deriving (Show, Eq)

-- Problem 11
encodeModified xs = map fixEncode $ encode' xs

fixEncode (1, x) = Single x
fixEncode (count, x) = Multiple count x

-- Proble 12
decodeModified [] = []
decodeModified (Single x : xs) = x : (decodeModified xs)
decodeModified (Multiple c x : xs) = take c (repeat x) ++ (decodeModified xs)

-- Problem 13
encodeDirect xs = []

-- Problem 14
dupli [] = []
dupli (x:xs) = x : x : (dupli xs)

-- Problem 15
repli [] n = []
repli (x:xs) n = take n (repeat x) ++ (repli xs n)

-- Problem 16
dropEvery xs n = []

-- Problem 17
split (xs) n = (take n xs, drop n xs)

-- Problem 18
slice list start end = []

-- Problem 19
rotate xs n = []

-- Problem 20
removeAt n (x:xs) = (x, xs)