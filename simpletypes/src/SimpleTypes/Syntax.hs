module SimpleTypes.Syntax where

import Control.Applicative

import SimpleTypes.Types

data Term = Var String 
          | VarI String Int
          | T
          | F
          | Unit
          | Cond Term Term Term
          | Abs String Type Term -- lambda abstraction with type
          | App Term Term
          | Let String Term Term
          | ParseError String
          deriving ( Eq, Show )

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



