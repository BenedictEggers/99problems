-- Ben Eggers
-- MIT Licensed

-- Problems 1-10

module MyLists
( myLast
, myLength
) where

myLast [x] = x
myLast (x:xs) = myLast xs

--myButLast []

myLength [] = 0
myLength (x:xs) = 1 + myLength xs