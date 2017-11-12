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
  arbitrary =  sized type' where 
    type' :: Int -> Gen Type
    type' 0 = return TUnit
    type' n = oneof [ return TUnit
                    , return TBool
                    , liftM2 TArr subtype  subtype 
                    ]
              where subtype = type' (n `div` 2)

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
                            {-, liftM4 Let arbitrary varString subterm subterm-}
                            ] 
              where subterm = term' (n `div` 2)
  

tests :: TestTree
tests =  testGroup "Parse Terms" 
  [ propertyTests
  , extendedSyntaxTests
  , parseValueTerms
  ]

propertyTests = testGroup "Test properties "
  [ testProperty "prop_parse"  prop_parse
  ]

extendedSyntaxTests = testGroup "Test parsing extended syntax"
  [ testCase "t_parseSeq_1" t_parseSeq_1
  , testCase "t_parseSeq_2" t_parseSeq_2
  , testCase "t_parseSeq_3" t_parseSeq_3
  ]

parseValueTerms = testGroup "Value Terms"
  [ testCase "t_parseTrue_1"   t_parseTrue_1
  , testCase "t_parseFalse_1"  t_parseFalse_1
  , testCase "t_parseUnit_1"   t_ParseUnit_1
  ]

prop_parse :: Term ->  Bool
prop_parse term  =  ( Right term ) ==  ( parseProgram . prettyprint ) term

t_parseSeq_1 = assertEqual "Failed to parse seq" 
  (Right "((\\ _:  Unit .  false ) true )") ( fmap prettyprint (parseProgram "(true;false)"))

t_parseSeq_2 = assertEqual "Failed to parse seq" 
        (Right "((\\ _:  Unit . ((\\ _:  Unit .  false ) false )) true )")
        (fmap prettyprint (parseProgram "(true;false;false)"))

t_parseSeq_3 = assertEqual "Failed to parse seq" 
        (Right "((\\ _:  Unit . ((\\ _:  Unit .  false ) false ))(\\ z:  Bool .  z ))")
        (fmap prettyprint (parseProgram "((\\z:Bool.z);false;false)"))

t_parseTrue_1  = assertEqual "Failed to parse 'True'"  ( Right $ T l )  $ parseProgram "true"
t_parseFalse_1 = assertEqual "Failed to parse 'False'" ( Right $ F l )  $ parseProgram "false"
t_ParseUnit_1  = assertEqual "Failed to parse 'Unit'"  (Right $ Unit l) $ parseProgram "unit"

