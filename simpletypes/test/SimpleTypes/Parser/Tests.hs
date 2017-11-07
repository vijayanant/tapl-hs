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
  {-, parseDerivedTerms-}
  ]

parseValueTerms = testGroup "Value Terms"
  [ testCase "parseTrue"   t_true_1
  , testCase "parseFalse"  t_false_1
  , testCase "parseUnit"   t_unit_1
  ]

parseSimpleTerms = testGroup "Simple Terms"
  [ testCase "parseIf1"     t_if_1
  , testCase "parseIf2"     t_if_2
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


iff = \t -> If t t F 
abstb = \t -> Abs "x" TBool t  

t_true_1  = assertEqual "Failed to parse 'True'"   T      $ parseProgram "True"
t_false_1 = assertEqual "Failed to parse 'False'"  F      $ parseProgram "False"
t_unit_1  = assertEqual "Failed to parse 'Unit'"  Unit  $ parseProgram "unit"

t_if_1 = assertEqual "Failed to parse 'If" (If T F T)  $ parseProgram "if True then False else True"

t_abs_1 = assertEqual "Failed to parse 'Abstraction'" (Abs "f" TBool T )  $ parseProgram "\\f:Bool.True"
t_abs_2 = assertEqual "Failed to parse 'Abstraction'" (Abs "f" TBool (Var "x"))  $ parseProgram "\\f:Bool.x"

t_abs_3 = assertEqual "Failed to parse 'Abstraction'" (Abs "f" TBool (If (Var "x") T F ))  $ 
  parseProgram "\\f:Bool.if x then True else False"

t_abs_4 = assertEqual "Failed to parse 'Abstraction'" (Abs "bool" TBool (If (Var "bool") (Var "bool") F ))  $ 
  parseProgram "\\bool:Bool.if bool then bool else False"

t_abs_5 = assertEqual "Failed to parse 'Abstraction'" (Abs "bool" TBool (If (Var "bool") (Var "bool") F ))  $ 
  parseProgram "\\bool:Bool.if bool then bool else False"

t_app_1 = assertEqual "Failed to parse 'Application'" (App (abstb (iff (Var "x"))) T) $
  parseProgram "(\\x:Bool.if x then x else False) True"

t_if_2 = assertEqual "Failed to parse 'If" (If (App (Abs "x" TBool  (Var "x")) T) F T)  $ parseProgram "if ((\\x:Bool.x) True) then False else True"
  
-------- Derived Terms ----------
t_seq_1 = assertEqual "Failed to parse 'Sequence'" (App (Abs "_" TUnit F) T) $
  parseProgram "(True;False)"

t_seq_2 = assertEqual "Failed to parse 'Sequence'" (App (Abs "_" TUnit (App (Abs "_" TUnit F) F )) T) $
  parseProgram "(True;(False;False))"
