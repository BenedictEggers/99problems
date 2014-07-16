-- Ben Eggers <ben.eggers36@gmail.com>
-- Tests for problems 1-10.

module MyListsTests (test1thru10) where

import MyLists
import Test.HUnit

main = runTestTT test1thru10

test1thru10 = TestList [testLast
                      , testButLast
                      , testElementAt
                      , testLength
                      , testReverse
                      , testIsPalindrome
                      , testFlatten
                      , testCompress
                      , testPack
                      , testEncode
                      ]

-- Problem 1
testLast = TestList [testLast1, testLast2, testLast3]
testLast1 = TestCase (assertEqual "3 should be the last element of [1,2,3]"
                        3 (last' [1,2,3]))
testLast2 = TestCase (assertEqual "4 should be the last element of [1,2,3,4]"
                        4 (last' [1,2,3,4]))
testLast3 = TestCase (assertEqual "'z' should be the last element of ['x','y','z']"
                        'z' (last' ['x','y','z']))

-- Problem 2
testButLast = TestList [testButLast1, testButLast2, testButLast3]
testButLast1 = TestCase (assertEqual "2 should be the 2nd-to-last of [1,2,3]"
                            2 (butLast' [1,2,3]))
testButLast2 = TestCase (assertEqual "3 should be the 2nd-to-last of [1,2,3,4]"
                            3 (butLast' [1,2,3,4]))
testButLast3 = TestCase (assertEqual "'y' should be the 2nd-to-last of ['a'..'z']"
                            'y' (butLast' ['a'..'z']))

-- Problem 3
testElementAt = TestList [testElementAt1, testElementAt2
                          , testElementAt3, testElementAt4]
testElementAt1 = TestCase (assertEqual "1 should be the first element of [1,2,3]"
                              1 (elementAt' [1,2,3] 1))
testElementAt2 = TestCase (assertEqual "2 should be the second element of [1,2,3]"
                              2 (elementAt' [1,2,3] 2))
testElementAt3 = TestCase (assertEqual "3 should be the third element of [1,2,3]"
                              3 (elementAt' [1,2,3] 3))
testElementAt4 = TestCase (assertEqual "'j' should be the 10th element of ['a'..'z']"
                              'j' (elementAt' ['a'..'z'] 10))

-- Problem 4
testLength = TestList [testLength1, testLength2, testLength3]
testLength1 = TestCase (assertEqual "Length of [] should be 0"
                          0 (length' []))
testLength2 = TestCase (assertEqual "Length of [1,2,3] should be 3"
                          3 (length' [1,2,3]))
testLength3 = TestCase (assertEqual "Length of 'hello, world!' should be 13"
                          13 (length' "hello, world!"))

-- Problem 5
testReverse = TestList [testReverse1, testReverse2, testReverse3, testReverse4]
testReverse1 = TestCase (assertEqual "[1] reversed should be [1]"
                            [1] (reverse' [1]))
testReverse2 = TestCase (assertEqual "[1,2] reversed should be [2,1]"
                            [2,1] (reverse' [1,2]))
testReverse3 = TestCase (assertEqual "['a'..'z'] reversed should be ['z'..'a']"
                            ['z', 'y'..'a'] (reverse' ['a'..'z']))
testReverse4 = TestCase (assertEqual "'A man, a plan, a canal, panama!' reversed should be \
                            \ '!amanap ,lanac a ,nalp a ,nam A'"
                            "!amanap ,lanac a ,nalp a ,nam A"
                            (reverse' "A man, a plan, a canal, panama!"))

-- Problem 6
testIsPalindrome = TestList [testIsPalindrome1, testIsPalindrome2, testIsPalindrome3]
testIsPalindrome1 = TestCase (assertEqual "[1,2,3] is not a palindrome"
                              False (isPalindrome' [1,2,3]))
testIsPalindrome2 = TestCase (assertEqual "'racecar' is a palindrome"
                              True (isPalindrome' "racecar"))
testIsPalindrome3 = TestCase (assertEqual "[1,2,4,8,16,8,4,2,1] is a palindrome"
                              True (isPalindrome' [1,2,4,8,16,8,4,2,1]))

-- Problem 7
testFlatten = TestList [testFlatten1, testFlatten2, testFlatten3]
testFlatten1 = TestCase (assertEqual "[5] should flatten to [5]"
                          [5] (flatten' (Elem 5)))
testFlatten2 = TestCase (assertEqual "[1, [2, 3]] should flatten to [1, 2, 3]"
                          [1,2,3] (flatten' (List [Elem 1, List [Elem 2, Elem 3]])))
testFlatten3 = TestCase (assertEqual
                          "[1, [2, [3, 4], 5]] should flatten to [1,2,3,4,5]"
                          [1,2,3,4,5]
                          (flatten' (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])))

-- Problem 8
testCompress = TestList [testCompress1, testCompress2, testCompress3]
testCompress1 = TestCase (assertEqual "'' should compress to ''"
                          "" (compress' ""))
testCompress2 = TestCase (assertEqual "\"aabccdee\" should compress to \"abcde\""
                          "abcde" (compress' "aabccdee"))
testCompress3 = TestCase (assertEqual "\"aaaaaabaaabbabaaabbbb\" should compress to \"abababab\""
                          "abababab" (compress' "aaaaaabaaabbabaaabbbb"))

-- Problem 9
testPack = TestList [testPack1, testPack2, testPack3]
testPack1 = TestCase (assertEqual "[1] should pack to [[1]]"
                      [[1]] (pack' [1]))
testPack2 = TestCase (assertEqual "[1,1,2,3] should pack to [[1, 1], [2], [3]]"
                      [[1, 1], [2], [3]] (pack' [1,1,2,3]))
testPack3 = TestCase (assertEqual "['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', \
                                  \ 'a', 'd', 'e', 'e', 'e', 'e'] should pack to \
                                  \ [\"aaaa\",\"b\",\"cc\",\"aa\",\"d\",\"eeee\"]"
                      ["aaaa","b","cc","aa","d","eeee"]
                      (pack' ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
                              'a', 'd', 'e', 'e', 'e', 'e']))

-- Problem 10
testEncode = TestList [testEncode1]
testEncode1 = TestCase (assertEqual "\"aaaabccaadeeee\" should encode to \
                                    \ [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]"
                                    [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
                                    (encode' "aaaabccaadeeee"))
