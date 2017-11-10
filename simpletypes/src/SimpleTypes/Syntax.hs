module SimpleTypes.Syntax where

import Control.Applicative
import SimpleTypes.Types

data Info = Info 
  { row :: Int
  , col :: Int
  } deriving Show

{-instance Show Info where-}
  {-show (Info l c) = concat ["[line: ", show l, ", col: ", show c, " ] "]-}

instance Eq Info where 
  (==) i1 i2 = True

data Term = Var Info String 
          | VarI Info String Int
          | T Info
          | F Info
          | Unit Info
          | Cond Info Term Term Term
          | Abs Info String Type Term -- lambda abstraction with type
          | App Info Term Term
          | Let Info String Term Term
          | ParseError String
          deriving (Eq, Show)

prettyprint :: Term -> String
prettyprint (Var x)       = concat [x] -- , ": ", prettyprint ty]
prettyprint (VarI x k)    = concat [x] --, ": ", prettyprint ty]
prettyprint (Abs x ty1 t) = concat ["(\\", x, ": ", show ty1, ". ", prettyprint t, ")"]
prettyprint (App t1 t2)   = concat ["(", prettyprint t1,  ") (", prettyprint t2, ")"]
prettyprint (Cond t1 t2 t3) = concat ["(Cond ", prettyprint t1, prettyprint t2, prettyprint t3, ")"]
prettyprint (Let x t1 t2) = concat ["Let ", x, " = ", prettyprint t1, " in ", prettyprint t2]
prettyprint (T)           = concat ["True"]
prettyprint (F)           = concat ["False"]
prettyprint (Unit)        = concat ["Unit"]
prettyprint (ParseError e)= e



