-- Ben Eggers <ben.eggers36@gmail.com>
-- Problems 1-10

module MyLists where

-- 1
last' [x] = x
last' (x:xs) = last' xs

-- 2
butLast' [x, y] = x
butLast' (x:xs) = butLast' xs

-- 3
elementAt' xs n
    | n == 1    = head xs
    | otherwise = elementAt' (tail xs) (n - 1)

-- 4
length' [] = 0
length' (x:xs) = 1 + length' xs

-- 5
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

-- 6
isPalindrome' xs = xs == (reverse' xs)

-- 7
data NestedList a = Elem a | List [NestedList a]
flatten' :: NestedList a -> [a]
flatten' list = []

-- 8
compress' stuff = []

-- 9
pack' stuff = []

-- 10
encode' stuff = []