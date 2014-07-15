-- Ben Eggers <ben.eggers36@gmail.com>
-- Tests!

module Tests where

import Test_MyLists( test1thru10 )
import Test_MoreLists( test11thru20 )
import Test.HUnit

main = runTestTT tests

tests = TestList [test1thru10
                , test11thru20]