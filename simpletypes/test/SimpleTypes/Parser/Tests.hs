module SimpleTypes.Parser.Tests 
( tests )
where 

import SimpleTypes.Parser
import SimpleTypes.Syntax
import SimpleTypes.Types

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)

tests :: TestTree
tests =  testGroup "Parse Terms" 
  [ parseValueTerms
  , parseSimpleTerms
  , parseDerivedTerms
  ]

parseValueTerms = testGroup "Value Terms"
  [ testCase "parseTrue"   t_true_1
  , testCase "parseFalse"  t_false_1
  , testCase "parseUnit"   t_unit_1
  ]

parseSimpleTerms = testGroup "Simple Terms"
  [ testCase "parseCond1"     t_cond_1
  , testCase "parseCond2"     t_cond_2
  , testCase "parseAbstraction1"    t_abs_1
  , testCase "parseAbstraction2"    t_abs_2
  , testCase "parseAbstraction3"    t_abs_3
  , testCase "parseAbstraction4"    t_abs_4
  , testCase "parseAbstraction5"    t_abs_5
  , testCase "parseApplication1"    t_app_1
  ]

parseDerivedTerms = testGroup "Derived Terms" 
  [ testCase "parseSequence1" t_seq_1
  , testCase "parseSequence2" t_seq_2
  ]


condf = \t -> Cond t t F 
abstb = \t -> Abs "x" TBool t  

t_true_1  = assertEqual "Failed to parse 'True'"  ( Right T )      $ parseProgram "True"
t_false_1 = assertEqual "Failed to parse 'False'" ( Right F )      $ parseProgram "False"
t_unit_1  = assertEqual "Failed to parse 'Unit'"  (Right Unit)     $ parseProgram "unit"

t_cond_1 = assertEqual "Failed to parse 'Cond" (Right (Cond T F T))  $ parseProgram "cond (True) (False)(True)"

t_abs_1 = assertEqual "Failed to parse 'Abstraction'" (Right (Abs "f" TBool T ))  $ parseProgram "\\f:Bool.True"
t_abs_2 = assertEqual "Failed to parse 'Abstraction'" (Right (Abs "f" TBool (Var "x")))  $ parseProgram "\\f:Bool.x"

t_abs_3 = assertEqual "Failed to parse 'Abstraction'" (Right (Abs "f" TBool (Cond (Var "x") T F )))  $ 
  parseProgram "\\f:Bool.cond ( x )  ( True )  ( False )"

t_abs_4 = assertEqual "Failed to parse 'Abstraction'" (Right (Abs "bool" TBool (Cond (Var "bool") (Var "bool") F )))  $ 
  parseProgram "\\bool:Bool.cond ( bool )  ( bool )  ( False )"

t_abs_5 = assertEqual "Failed to parse 'Abstraction'" (Right (Abs "bool" TBool (Cond (Var "bool") (Var "bool") F )))  $ 
  parseProgram "\\bool:Bool.cond ( bool )  ( bool )  ( False )"

t_app_1 = assertEqual "Failed to parse 'Application'" (Right (App (abstb (condf (Var "x"))) T)) $
  parseProgram "(\\x:Bool.cond ( x )  ( x )  ( False )) True"

t_cond_2 = assertEqual "Failed to parse 'Cond" (Right (Cond (App (Abs "x" TBool  (Var "x")) T) F T))  $ parseProgram "cond ((\\x:Bool.x) True) ( False ) ( True )"



-------------------------------------------------------------------------
--                    Derived Terms
------------------------------------------------------------------------



t_seq_1 = assertEqual "Failed to parse 'Sequence'" (Right (App (Abs "_" TUnit F) T)) $
  parseProgram "(True;False)"

t_seq_2 = assertEqual "Failed to parse 'Sequence'" (Right (App (Abs "_" TUnit (App (Abs "_" TUnit F) F )) T)) $
  parseProgram "(True;(False;False))"
