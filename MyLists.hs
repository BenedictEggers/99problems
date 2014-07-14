-- Ben Eggers
-- MIT Licensed

-- Problems 1-10

module MyLists
( myLast
, myButLast
, myElementAt
, myLength
) where

myLast [x] = x
myLast (x:xs) = myLast xs

myButLast [x, y] = x
myButLast (x:xs) = myButLast xs

myElementAt xs n
    | n == 1    = head xs
    | otherwise = myElementAt (tail xs) (n - 1)

myLength [] = 0
myLength (x:xs) = 1 + myLength xs