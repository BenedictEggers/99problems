
module Tests where

import MyLists( myLast, myLength )
import Test.HUnit

main = runTestTT all_tests

all_tests = TestList [test1thru10]

test1thru10 = TestList [testMyLast1, testMyLast2, testMyLast3
                        , testMyLength1, testMyLength2, testMyLength3]

-- Problem 1
testMyLast1 = TestCase (assertEqual "3 should be the last element of [1,2,3]"
                        3 (myLast [1,2,3]))
testMyLast2 = TestCase (assertEqual "4 should be the last element of [1,2,3,4]"
                        4 (myLast [1,2,3,4]))
testMyLast3 = TestCase (assertEqual "'z' should be the last element of ['x','y','z']"
                        'z' (myLast ['x','y','z']))

-- Problem 4
testMyLength1 = TestCase (assertEqual "Length of [] should be 0"
                          0 (myLength []))
testMyLength2 = TestCase (assertEqual "Length of [1,2,3] should be 3"
                          3 (myLength [1,2,3]))
testMyLength3 = TestCase (assertEqual "Length of 'hello, world!' should be 13"
                          13 (myLength "hello, world!"))