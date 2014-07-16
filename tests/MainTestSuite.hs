-- Ben Eggers <ben.eggers36@gmail.com>
-- Tests!

module Main (
  main
) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import MyListsTests( test1thru10 )
import MoreListsTests( test11thru20 )
--import EvenMoreListsTests ( test21thru28 )

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