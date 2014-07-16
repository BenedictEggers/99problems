-- Ben Eggers <ben.eggers36@gmail.com>
-- Tests!

module Main (
  main
) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import Test_MyLists( test1thru10 )
import Test_MoreLists( test11thru20 )
--import Test_EvenMoreLists ( test21thru28 )

main = defaultMain tests

tests :: [Test]
tests = 
  [
    testGroup "Test"
    [
      testGroup "1-10" $ hUnitTestToTests test1thru10
    , testGroup "11-20" $ hUnitTestToTests test11thru20
    ]
  ]