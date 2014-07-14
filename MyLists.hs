-- Ben Eggers <ben.eggers36@gmail.com>
-- Problems 1-10

module MyLists
( myLast
, myButLast
, myElementAt
, myLength
, myReverse
, isPalindrome
, myFlatten
) where

-- 1
myLast [x] = x
myLast (x:xs) = myLast xs

-- 2
myButLast [x, y] = x
myButLast (x:xs) = myButLast xs

-- 3
myElementAt xs n
    | n == 1    = head xs
    | otherwise = myElementAt (tail xs) (n - 1)

-- 4
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- 5
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- 6
isPalindrome xs = xs == (myReverse xs)

-- 7
data NestedList a = Elem a | List [NestedList a]
myFlatten = 7