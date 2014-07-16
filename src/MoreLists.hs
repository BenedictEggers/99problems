-- Ben Eggers <ben.eggers36@gmail.com>
-- Problems 11-20

module MoreLists where

import MyLists( encode' )

data Encoded a = Single a | Multiple Int a deriving (Show, Eq)

-- Problem 11
encodeModified xs = map fixEncode $ encode' xs
  where
    fixEncode (1, x) = Single x
    fixEncode (count, x) = Multiple count x

-- Proble 12
decodeModified [] = []
decodeModified (Single x : xs) = x : (decodeModified xs)
decodeModified (Multiple c x : xs) = (replicate c x) ++ (decodeModified xs)

-- Problem 13
encodeDirect [] = []
encodeDirect (x:xs) = fixOnes (length (takeWhile (==x) xs)+1, x) :
                                  encodeDirect (dropWhile (==x) xs)
                      where fixOnes (1, x) = Single x
                            fixOnes (c, x) = Multiple c x

-- Problem 14
dupli [] = []
dupli (x:xs) = x : x : (dupli xs)

-- Problem 15
repli [] n = []
repli (x:xs) n = (replicate n x) ++ (repli xs n)

-- Problem 16
dropEvery [] n = []
dropEvery xs n = dropHelper xs n n
                where dropHelper [] _ _ = []
                      dropHelper (x:xs) 1 n = dropHelper xs n n
                      dropHelper (x:xs) c n = x : (dropHelper xs (c-1) n)

-- Problem 17
split xs n = (take n xs, drop n xs)

-- Problem 18
slice xs start end = take (end - start + 1) $ drop (start - 1) xs

-- Problem 19
rotate xs n
    | n >= 0 = let (start, end) = split xs n in end ++ start
    | otherwise = reverse $ rotate (reverse xs) (-n)

-- Problem 20
removeAt n xs = (xs !! (n - 1), (slice xs 1 (n - 1)) ++ (slice xs (n + 1) (length xs)))