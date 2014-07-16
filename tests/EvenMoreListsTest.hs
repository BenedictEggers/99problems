-- Ben Eggers
-- Problem 21-28 tests

module EvenMoreListsTests where

import EvenMoreLists
import Test.HUnit

main = runTestTT test21thru28

test21thru28 = TestList [testInsertAt
                        , testRange'
                        , testRndSelect
                        , testDiffSelect
                        , testRndPermu
                        , testCombinations
                        , testGroup'
                        , testLSort
                        , testLFSort]

-- Problem 21
testInsertAt = TestList [testInsertAt1, testInsertAt2, testInsertAt3]
testInsertAt1 = TestCase (assertEqual "insertAt 4 [1,2,3] 2 should give [1, 4, 2, 3]"
                          [1,4,2,3] (insertAt 4 [1,2,3] 2))
testInsertAt2 = TestCase (assertEqual "insertAt 10 [1..10] 1 should give 10:[1..10]"
                          10:[1..10] (insertAt 10 [1..10] 1))
testInsertAt3 = TestCase (assertEqual)

testRange' = TestList []

testRndSelect = TestList []

testDiffSelect = TestList []

testRndPermu = TestList []

testCombinations = TestList []

testGroup' = TestList []

testLSort = TestList []

testLFSort = TestList []
