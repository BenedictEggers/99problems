-- Ben Eggers <ben.eggers36@gmail.com>
-- Tests for problems 11-20. Note: I think this test harness isn't laid out
-- in the proper HUnit style, but I couldn't find accessible guides online. Drop
-- me a line if you have constructive criticism.

 --TODO: Auto-populate test lists?

module Test_MoreLists where

import MoreLists
import Test.HUnit

test11thru20 = TestList [testEncodeModified
                        , testDecodeModified
                        , testEncodeDirect
                        , testDupli
                        , testRepli
                        , testDropEvery
                        , testSplit
                        , testSlice
                        , testRotate
                        , testRemoveAt]

-- Problem 1
testEncodeModified = TestList[testEncodeModified1, testEncodeModified2, testEncodeModified]
testEncodeModified1 = TestCase (assertEqual "\"a\" should encode to [Single 'a']"
                                [Single 'a'] (encodeModified "a"))
testEncodeModified2 = TestCase (assertEqual "\"aab\" should encode to \
                                \ [Multiple 2 'a', Single 'b']"
                                [Multiple 2 'a', Single 'b'] (encodeModified "aab"))
testEncodeModified3 = TestCase (assertEqual "\"aaaabccaadeeee\" should encode \
                                \ to [Multiple 4 'a',Single 'b',Multiple 2 'c', \
                                \ Multiple 2 'a',Single 'd',Multiple 4 'e']"
                                [Multiple 4 'a', Single 'b', Multiple 2 'c',
                                Multiple 2 'a', Single 'd', Multiple 4 'e']
                                (encodeModified "aaaabccaadeeee"))

-- Problem 2
testDecodeModified = TestList[testDecodeModified1, testDecodeModified2, testDecodeModified]
testDecodeModified1 = TestCase (assertEqual "[Single 'a'] should decode to \"a\""
                                "a" (decodeModified [Single 'a']))
testDecodeModified2 = TestCase (assertEqual "[Multiple 2 'a', Single 'b'] should \
                                \ decode to \"aab\""
                                "aab" (decodeModified [Multiple 2 'a', Single 'b']))
testDecodeModified3 = TestCase (assertEqual "[Multiple 4 'a', Single 'b', Multiple 2 'c', \
                                \ Multiple 2 'a', Single 'd', Multiple 4 'e'] should \
                                \ decode to \"aaaabccaadeeee\""
                                "aaaabccaadeeee"
                                (decodeModified [Multiple 4 'a', Single 'b', Multiple 2 'c',
                                Multiple 2 'a', Single 'd', Multiple 4 'e']))

-- Problem 3
testEncodeDirect = TestList[testEncodeDirect1, testEncodeDirect2, testEncodeDirect]
testEncodeDirect1 = TestCase (assertEqual)
testEncodeDirect2 = TestCase (assertEqual)
testEncodeDirect3 = TestCase (assertEqual)

-- Problem 4
testDupli = TestList[testDupli1, testDupli2, testDupli]
testDupli1 = TestCase (assertEqual)
testDupli2 = TestCase (assertEqual)
testDupli3 = TestCase (assertEqual)

-- Problem 5
testRepli = TestList[testRepli1, testRepli2, testRepli]
testRepli1 = TestCase (assertEqual)
testRepli2 = TestCase (assertEqual)
testRepli3 = TestCase (assertEqual)

-- Problem 6
testDropEvery = TestList[testDropEvery1, testDropEvery2, testDropEvery]
testDropEvery1 = TestCase (assertEqual)
testDropEvery2 = TestCase (assertEqual)
testDropEvery3 = TestCase (assertEqual)

-- Problem 7
testSplit = TestList[testSplit1, testSplit2, testSplit]
testSplit1 = TestCase (assertEqual)
testSplit2 = TestCase (assertEqual)
testSplit3 = TestCase (assertEqual)

-- Problem 8
testSlice = TestList[testSlice1, testSlice2, testSlice]
testSlice1 = TestCase (assertEqual)
testSlice2 = TestCase (assertEqual)
testSlice3 = TestCase (assertEqual)

-- Problem 9
testRotate = TestList[testRotate1, testRotate2, testRotate]
testRotate1 = TestCase (assertEqual)
testRotate2 = TestCase (assertEqual)
testRotate3 = TestCase (assertEqual)

-- Problem 10
testRemoveAt = TestList[testRemoveAt1, testRemoveAt2, testRemoveAt]
testRemoveAt1 = TestCase (assertEqual)
testRemoveAt2 = TestCase (assertEqual)
testRemoveAt3 = TestCase (assertEqual)