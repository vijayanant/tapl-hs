module SimpleTypes.Parser.Tests 
( tests )
where 

import SimpleTypes.Parser
import SimpleTypes.Syntax
import SimpleTypes.Types

import Control.Monad 

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)
import Test.Tasty.QuickCheck
import Test.QuickCheck.Arbitrary

-- dummy location info for generators
-- location values are not considered for equality tests 
l = Info 0 0 

-- generates lower case strins (of length >= 1)
varString :: Gen String
varString = suchThat (listOf ( elements ['a'..'z'])) (\s -> length s > 0)

instance Arbitrary Info where 
  arbitrary = return l

instance Arbitrary Type where 
  arbitrary = return TBool

instance Arbitrary Term where 
  arbitrary = sized term' where 
    term' :: Int -> Gen Term
    term' 0  = liftM T arbitrary
    term' n  = oneof [ liftM2 Var arbitrary varString
                            , liftM T arbitrary
                            , liftM F arbitrary
                            , liftM Unit arbitrary
                            , liftM4 Cond arbitrary subterm subterm subterm 
                            , liftM4 Abs  arbitrary varString arbitrary subterm
                            , liftM3 App arbitrary subterm subterm
                            {-, liftM4 Let arbitrary arbitrary subterm subterm-}
                            ] 
              where subterm = term' (n `div` 2)
  

tests :: TestTree
tests =  testGroup "Parse Terms" 
  [ propertyTests
  , parseValueTerms
  ]

propertyTests = testGroup "Test properties "
  [ testProperty "parse = parse . prettyprint . parse"  prop_parse
  ]

parseValueTerms = testGroup "Value Terms"
  [ testCase "parseTrue"   t_true_1
  , testCase "parseFalse"  t_false_1
  , testCase "parseUnit"   t_unit_1
  ]

prop_parse :: Term ->  Bool
prop_parse term  =  ( Right term ) ==  ( parseProgram . prettyprint ) term

t_true_1  = assertEqual "Failed to parse 'True'"  ( Right $ T l )      $ parseProgram "true"
t_false_1 = assertEqual "Failed to parse 'False'" ( Right $ F l )      $ parseProgram "false"
t_unit_1  = assertEqual "Failed to parse 'Unit'"  (Right $ Unit l)     $ parseProgram "unit"

