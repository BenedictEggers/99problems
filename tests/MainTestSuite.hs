-- Ben Eggers <ben.eggers36@gmail.com>
-- Tests!

module Main (
  main
) where

import Test.Framework as TF (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import MyLists
import MoreLists

main = defaultMain tests

tests :: [TF.Test]
tests = 
  [
    testGroup "last' (problem 1)" [
      testCase "3 should be the last element of [1,2,3]"
                (3 @=? last' [1,2,3]),
      testCase "4 should be the last element of [1,2,3,4]"
                (4 @=? last' [1,2,3,4]),
      testCase "'z' should be the last element of ['x','y','z']"
                ('z' @=? last' ['x','y','z'])
    ],

    testGroup "butLast' (problem 2)" [
      testCase "2 should be the 2nd-to-last of [1,2,3]" 
          (2 @=? butLast' [1,2,3]),
      testCase "3 should be the 2nd-to-last of [1,2,3,4]" 
          (3 @=? butLast' [1,2,3,4]),
      testCase "'y' should be the 2nd-to-last of ['a'..'z']"
          ('y' @=? butLast' ['a'..'z'])
    ],

    testGroup "elementAt' (problem 3)" [
      testCase "1 should be the first element of [1,2,3]" 
          (1 @=? elementAt' [1,2,3] 1),
      testCase "2 should be the second element of [1,2,3]" 
          (2 @=? elementAt' [1,2,3] 2),
      testCase "3 should be the third element of [1,2,3]" 
          (3 @=? elementAt' [1,2,3] 3),
      testCase "'j' should be the 10th element of ['a'..'z']" 
          ('j' @=? elementAt' ['a'..'z'] 10)
    ],

    testGroup "length' (problem 4)" [
      testCase "Length of [] should be 0"
          (0 @=? length' []),
      testCase "Length of [1,2,3] should be 3" 
          (3 @=? length' [1,2,3]),
      testCase "Length of 'hello, world!' should be 13"
          (13 @=? length' "hello, world!")
    ],

    testGroup "reverse' (problem 5)" [
      testCase "[1] reversed should be [1]" 
          ([1] @=? reverse' [1]),
      testCase "[1,2] reversed should be [2,1]"
          ([2,1] @=? reverse' [1,2]),
      testCase "['a'..'z'] reversed should be ['z'..'a']"
          (['z', 'y'..'a'] @=? reverse' ['a'..'z']),
      testCase "'A man, a plan, a canal, panama!' reversed should be \
                \ '!amanap ,lanac a ,nalp a ,nam A'"
          ("!amanap ,lanac a ,nalp a ,nam A" @=? reverse' "A man, a plan, a canal, panama!")
    ],

    testGroup "isPalindrome' (problem 6)" [
      testCase "[1,2,3] is not a palindrome"
          (False @=? isPalindrome' [1,2,3]),
      testCase "'racecar' is a palindrome"
          (True @=? isPalindrome' "racecar"),
      testCase "[1,2,4,8,16,8,4,2,1] is a palindrome"
          (True @=? isPalindrome' [1,2,4,8,16,8,4,2,1])
    ],

    testGroup "flatten' (problem 7)" [
      testCase "[5] should flatten to [5]"
          ([5] @=? flatten' (Elem 5)),
      testCase "[1, [2, 3]] should flatten to [1, 2, 3]"
          ([1,2,3] @=? flatten' (List [Elem 1, List [Elem 2, Elem 3]])),
      testCase "[1, [2, [3, 4], 5]] should flatten to [1,2,3,4,5]"
          ([1,2,3,4,5] @=?
            flatten' (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]))
    ],

    testGroup "compress' (problem 8)" [
      testCase "'' should compress to ''"
          ("" @=? compress' ""),
      testCase "\"aabccdee\" should compress to \"abcde\""
          ("abcde" @=? compress' "aabccdee"),
      testCase "\"aaaaaabaaabbabaaabbbb\" should compress to \"abababab\""
          ("abababab" @=? compress' "aaaaaabaaabbabaaabbbb")
    ],

    testGroup "pack' (problem 9)" [
      testCase "[1] should pack to [[1]]"
          ([[1]] @=? pack' [1]),
      testCase "[1,1,2,3] should pack to [[1, 1], [2], [3]]"
          ([[1, 1], [2], [3]] @=? pack' [1,1,2,3]),
      testCase "\"aaaabccaadeeee\" should pack to \
        \ [\"aaaa\",\"b\",\"cc\",\"aa\",\"d\",\"eeee\"]"
          (["aaaa","b","cc","aa","d","eeee"] @=? pack' "aaaabccaadeeee")
    ],

    testGroup "encode' (problem 10)" [
      testCase "\"aaaabccaadeeee\" should encode to \
      \ [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]"
          ([(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')] @=?
          encode' "aaaabccaadeeee")
    ],

    testGroup "encodeModified (problem 11)" [
      testCase "\"a\" should encode to [Single 'a']"
          ([Single 'a'] @=? encodeModified "a"),
      testCase "\"aab\" should encode to \ \ [Multiple 2 'a', Single 'b']"
          ([Multiple 2 'a', Single 'b'] @=? encodeModified "aab"),
      testCase "\"aaaabccaadeeee\" should encode \
                \ to [Multiple 4 'a',Single 'b',Multiple 2 'c', \
                \ Multiple 2 'a',Single 'd',Multiple 4 'e']"
          ([Multiple 4 'a', Single 'b', Multiple 2 'c', 
            Multiple 2 'a', Single 'd', Multiple 4 'e'] @=?
          encodeModified "aaaabccaadeeee")
    ],

    testGroup "decodeModified (problem 12)" [
      testCase "[Single 'a'] should decode to \"a\""
          ("a" @=? decodeModified [Single 'a']),
      testCase "[Multiple 2 'a', Single 'b'] should \ \ decode to \"aab\""
          ("aab" @=? decodeModified [Multiple 2 'a', Single 'b']), 
      testCase "[Multiple 4 'a', Single 'b', Multiple 2 'c', \
                \ Multiple 2 'a', Single 'd', Multiple 4 'e'] should \
                \ decode to \"aaaabccaadeeee\""
          ("aaaabccaadeeee" @=?
          decodeModified [Multiple 4 'a', Single 'b', Multiple 2 'c',
          Multiple 2 'a', Single 'd', Multiple 4 'e'])
    ],

    testGroup "encodeDirect (problem 13)" [
      testCase "\"a\" should encode to [Single 'a']"
          ([Single 'a'] @=? encodeDirect "a"),
      testCase "\"aab\" should encode to \ \ [Multiple 2 'a', Single 'b']"
          ([Multiple 2 'a', Single 'b'] @=? encodeDirect "aab"),
      testCase "\"aaaabccaadeeee\" should encode \
                \ to [Multiple 4 'a',Single 'b',Multiple 2 'c', \
                \ Multiple 2 'a',Single 'd',Multiple 4 'e']"
          ([Multiple 4 'a', Single 'b', Multiple 2 'c',
            Multiple 2 'a', Single 'd', Multiple 4 'e'] @=?
          encodeDirect "aaaabccaadeeee")
    ],

    testGroup "dupli (problem 14)" [
      testCase "[1] should dupli to [1, 1]"
          ([1, 1] @=? dupli [1]),
      testCase "[1, 2, 3] should dupli to [1, 1, 2, 2, 3, 3]" 
          ([1,1,2,2,3,3] @=? dupli [1,2,3]),
      testCase "[1,1,1] should dupli to [1, 1, 1, 1, 1, 1]" 
          ([1,1,1,1,1,1] @=? dupli [1,1,1])
    ],

    testGroup "repli (Problem 15)" [
      testCase "repli [1,2,3] 1 should give [1,2,3]"
          ([1,2,3] @=? repli [1,2,3] 1),
      testCase "repli \"abc\" 3 should give \"aaabbbccc\""
          ("aaabbbccc" @=? repli "abc" 3),
      testCase "repli [1] 5 should give [1,1,1,1,1]"
          ([1,1,1,1,1] @=? repli [1] 5)
    ],

    testGroup "dropEvery (Problem 16)" [
      testCase "dropEvery \"aoeu\" 1 should give the empty list"
          (([]::[Char]) @=? dropEvery "aoeu" 1),
      testCase "dropEvery \"abcdefghik\" 3 should give \"abdeghk\""
          ("abdeghk" @=? dropEvery "abcdefghik" 3),
      testCase "dropEvery [1,2,3] 4 should give [1,2,3]"
          ([1,2,3] @=? dropEvery [1,2,3] 4)
    ],

    testGroup "split (Problem 17)" [
      testCase "split \"abcdefghik\" 3 should give (\"abc\", \"defghik\")"
          (("abc", "defghik") @=? split "abcdefghik" 3)
    ],

    testGroup "slice (Problem 18)" [
      testCase "slice ['a'..'z'] 3 7 should give \"cdefg\""
          ("cdefg" @=? slice ['a'..'z'] 3 7),
      testCase "slice [1..10] 1 10 should give [1..10]"
          ([1..10] @=? slice [1..10] 1 10),
      testCase "slice [1..10] 1 1 should give [1]"
          ([1] @=? slice [1..10] 1 1)
    ],

    testGroup "rotate (Problem 19)" [
      testCase "rotate [1..10] 0 should give [1..10]"
          ([1..10] @=? rotate [1..10] 0),
      testCase "rotate ['a'..'h'] 3 should give \"defghabc\""
          ("defghabc" @=? rotate ['a'..'h'] 3),
      testCase "rotate ['a'..'h'] (-2) should give \"ghabcdef\""
          ("ghabcdef" @=? rotate ['a'..'h'] (-2))
    ],

    testGroup "removeAt (Problem 20)" [
      testCase "removeAt 2 \"abcd\" should give ('b', \"acd\")"
          (('b', "acd") @=? removeAt 2 "abcd"),
      testCase "removeAt 1 [1..10] should give (1, [2..10])"
          ((1, [2..10]) @=? removeAt 1 [1..10]),
      testCase "removeAt 6 [1..10] should give (6, [1..5]++[7..10])"
          ((6, [1..5]++[7..10]) @=? removeAt 6 [1..10])
    ]
  ]
