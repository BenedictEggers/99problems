-- Ben Eggers <ben.eggers36@gmail.com>
-- Tests!

module Tests where

import MyLists_tests( test1thru10 )
import Test.HUnit

main = runTestTT tests

tests = TestList [test1thru10]