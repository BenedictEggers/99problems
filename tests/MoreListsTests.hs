-- Ben Eggers <ben.eggers36@gmail.com>
-- Tests for problems 11-20.

module MoreListsTests where

import MoreLists
import Test.HUnit

main = runTestTT test11thru20

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

-- Problem 11
testEncodeModified = TestList[testEncodeModified1, testEncodeModified2, testEncodeModified3]
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

-- Problem 12
testDecodeModified = TestList[testDecodeModified1, testDecodeModified2, testDecodeModified3]
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

-- Problem 13
testEncodeDirect = TestList[testEncodeDirect1, testEncodeDirect2, testEncodeDirect3]
testEncodeDirect1 = TestCase (assertEqual "\"a\" should encode to [Single 'a']"
                                [Single 'a'] (encodeDirect "a"))
testEncodeDirect2 = TestCase (assertEqual "\"aab\" should encode to \
                                \ [Multiple 2 'a', Single 'b']"
                                [Multiple 2 'a', Single 'b'] (encodeDirect "aab"))
testEncodeDirect3 = TestCase (assertEqual "\"aaaabccaadeeee\" should encode \
                                \ to [Multiple 4 'a',Single 'b',Multiple 2 'c', \
                                \ Multiple 2 'a',Single 'd',Multiple 4 'e']"
                                [Multiple 4 'a', Single 'b', Multiple 2 'c',
                                Multiple 2 'a', Single 'd', Multiple 4 'e']
                                (encodeDirect "aaaabccaadeeee"))

-- Problem 14
testDupli = TestList[testDupli1, testDupli2, testDupli3]
testDupli1 = TestCase (assertEqual "[1] should dupli to [1, 1]"
                        [1, 1] (dupli [1]))
testDupli2 = TestCase (assertEqual "[1, 2, 3] should dupli to [1, 1, 2, 2, 3, 3]"
                        [1,1,2,2,3,3] (dupli [1,2,3]))
testDupli3 = TestCase (assertEqual "[1,1,1] should dupli to [1, 1, 1, 1, 1, 1]"
                        [1,1,1,1,1,1] (dupli [1,1,1]))

-- Problem 15
testRepli = TestList[testRepli1, testRepli2, testRepli3]
testRepli1 = TestCase (assertEqual "repli [1,2,3] 1 should give [1,2,3]"
                        [1,2,3] (repli [1,2,3] 1))
testRepli2 = TestCase (assertEqual "repli \"abc\" 3 should give \"aaabbbccc\""
                        "aaabbbccc" (repli "abc" 3))
testRepli3 = TestCase (assertEqual "repli [1] 5 should give [1,1,1,1,1]"
                        [1,1,1,1,1] (repli [1] 5))

-- Problem 16
testDropEvery = TestList[testDropEvery1, testDropEvery2, testDropEvery3]
testDropEvery1 = TestCase (assertEqual "dropEvery \"aoeu\" 1 should give the empty list"
                            ([]::[Char]) (dropEvery "aoeu" 1))
testDropEvery2 = TestCase (assertEqual "dropEvery \"abcdefghik\" 3 should give \"abdeghk\""
                            "abdeghk" (dropEvery "abcdefghik" 3))
testDropEvery3 = TestCase (assertEqual "dropEvery [1,2,3] 4 should give [1,2,3]"
                            [1,2,3] (dropEvery [1,2,3] 4))

-- Problem 17
testSplit = TestList[testSplit1]
testSplit1 = TestCase (assertEqual "split \"abcdefghik\" 3 should give (\"abc\", \"defghik\")"
                        ("abc", "defghik") (split "abcdefghik" 3))

-- Problem 18
testSlice = TestList[testSlice1, testSlice2, testSlice3]
testSlice1 = TestCase (assertEqual "slice ['a'..'z'] 3 7 should give \"cdefg\""
                        "cdefg" (slice ['a'..'z'] 3 7))
testSlice2 = TestCase (assertEqual "slice [1..10] 1 10 should give [1..10]"
                        [1..10] (slice [1..10] 1 10))
testSlice3 = TestCase (assertEqual "slice [1..10] 1 1 should give [1]"
                        [1] (slice [1..10] 1 1))

-- Problem 19
testRotate = TestList[testRotate1, testRotate2, testRotate3]
testRotate1 = TestCase (assertEqual "rotate [1..10] 0 should give [1..10]"
                        [1..10] (rotate [1..10] 0))
testRotate2 = TestCase (assertEqual "rotate ['a'..'h'] 3 should give \"defghabc\""
                        "defghabc" (rotate ['a'..'h'] 3))
testRotate3 = TestCase (assertEqual "rotate ['a'..'h'] (-2) should give \"ghabcdef\""
                        "ghabcdef" (rotate ['a'..'h'] (-2)))

-- Problem 20
testRemoveAt = TestList[testRemoveAt1, testRemoveAt2, testRemoveAt3]
testRemoveAt1 = TestCase (assertEqual "removeAt 2 \"abcd\" should give ('b', \"acd\")"
                            ('b', "acd") (removeAt 2 "abcd"))
testRemoveAt2 = TestCase (assertEqual "removeAt 1 [1..10] should give (1, [2..10])"
                            (1, [2..10]) (removeAt 1 [1..10]))
testRemoveAt3 = TestCase (assertEqual "removeAt 6 [1..10] should give (6, [1..5]++[7..10])"
                            (6, [1..5]++[7..10]) (removeAt 6 [1..10]))