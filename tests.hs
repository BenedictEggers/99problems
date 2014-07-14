-- Ben Eggers
-- Tests for all my solutions

 --TODO: Auto-populate test lists?

module Tests where

import MyLists( myLast, myButLast, myElementAt, myLength, myReverse, isPalindrome )
import Test.HUnit

main = runTestTT all_tests

all_tests = TestList [test1thru10]

test1thru10 = TestList [testMyLast
                      , testMyButLast
                      , testMyElementAt
                      , testMyLength
                      , testMyReverse
                      , testIsPalindrome]

-- Problem 1
testMyLast = TestList [testMyLast1, testMyLast2, testMyLast3]
testMyLast1 = TestCase (assertEqual "3 should be the last element of [1,2,3]"
                        3 (myLast [1,2,3]))
testMyLast2 = TestCase (assertEqual "4 should be the last element of [1,2,3,4]"
                        4 (myLast [1,2,3,4]))
testMyLast3 = TestCase (assertEqual "'z' should be the last element of ['x','y','z']"
                        'z' (myLast ['x','y','z']))

-- Problem 2
testMyButLast = TestList [testMyButLast1, testMyButLast2, testMyButLast3]
testMyButLast1 = TestCase (assertEqual "2 should be the 2nd-to-last of [1,2,3]"
                            2 (myButLast [1,2,3]))
testMyButLast2 = TestCase (assertEqual "3 should be the 2nd-to-last of [1,2,3,4]"
                            3 (myButLast [1,2,3,4]))
testMyButLast3 = TestCase (assertEqual "'y' should be the 2nd-to-last of ['a'..'z']"
                            'y' (myButLast ['a'..'z']))

-- Problem 3
testMyElementAt = TestList [testMyElementAt1, testMyElementAt2
                          , testMyElementAt3, testMyElementAt4]
testMyElementAt1 = TestCase (assertEqual "1 should be the first element of [1,2,3]"
                              1 (myElementAt [1,2,3] 1))
testMyElementAt2 = TestCase (assertEqual "2 should be the second element of [1,2,3]"
                              2 (myElementAt [1,2,3] 2))
testMyElementAt3 = TestCase (assertEqual "3 should be the third element of [1,2,3]"
                              3 (myElementAt [1,2,3] 3))
testMyElementAt4 = TestCase (assertEqual "'j' should be the 10th element of ['a'..'z']"
                              'j' (myElementAt ['a'..'z'] 10))

-- Problem 4
testMyLength = TestList [testMyLength1, testMyLength2, testMyLength3]
testMyLength1 = TestCase (assertEqual "Length of [] should be 0"
                          0 (myLength []))
testMyLength2 = TestCase (assertEqual "Length of [1,2,3] should be 3"
                          3 (myLength [1,2,3]))
testMyLength3 = TestCase (assertEqual "Length of 'hello, world!' should be 13"
                          13 (myLength "hello, world!"))

-- Problem 5
testMyReverse = TestList [testMyReverse1, testMyReverse2, testMyReverse3, testMyReverse4]
testMyReverse1 = TestCase (assertEqual "[1] reversed should be [1]"
                            [1] (myReverse [1]))
testMyReverse2 = TestCase (assertEqual "[1,2] reversed should be [2,1]"
                            [2,1] (myReverse [1,2]))
testMyReverse3 = TestCase (assertEqual "['a'..'z'] reversed should be ['z'..'a']"
                            ['z', 'y'..'a'] (myReverse ['a'..'z']))
testMyReverse4 = TestCase (assertEqual "'A man, a plan, a canal, panama!' reversed should be \
                            \ '!amanap ,lanac a ,nalp a ,nam A'"
                            "!amanap ,lanac a ,nalp a ,nam A"
                            (myReverse "A man, a plan, a canal, panama!"))

-- Problem 6
testIsPalindrome = TestList []