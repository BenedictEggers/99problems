-- Ben Eggers <ben.eggers36@gmail.com>
-- Problems 1-10

module MyLists where

-- Problem 1
last' [x] = x
last' (x:xs) = last' xs

-- Problem 2
butLast' [x, y] = x
butLast' (x:xs) = butLast' xs

-- Problem 3
elementAt' xs n
    | n == 1    = head xs
    | otherwise = elementAt' (tail xs) (n - 1)

-- Problem 4
length' [] = 0
length' (x:xs) = 1 + length' xs

-- Problem 5
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

-- Problem 6
isPalindrome' xs = xs == (reverse' xs)

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten' list = []

-- Problem 8
compress' [] = []
compress' [x] = [x]
compress' (x:xs)
    | x == head xs    = compress' xs
    | otherwise       = x : (compress' xs)

-- Problem 9
pack' stuff = []

-- Problem 10
encode' stuff = []