module SimpleTypes.Tests 
( tests )
where 

import qualified SimpleTypes.Parser.Tests as PT

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)

tests :: TestTree
tests = testGroup "SimpleTypes.Parser"
          [ PT.tests
          {-, SimpleTypes.TypeChecker.Tests.tests-}
          {-, SimpleTypes.Tests.tests-}
          ]

