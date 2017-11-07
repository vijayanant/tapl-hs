module SimpleTypes.Syntax where

import Control.Applicative

import SimpleTypes.Types

data Term = Var String 
          | VarI String Int
          | T
          | F
          | If Term Term Term
          | Abs String Type Term -- lambda abstraction with type
          | App Term Term
          | Unit
          | ParseError String
          deriving ( Eq, Show )

prettyprint :: Term -> String
prettyprint (Var x)       = concat [x] -- , ": ", prettyprint ty]
prettyprint (VarI x k)    = concat [x] --, ": ", prettyprint ty]
prettyprint (Abs x ty1 t) = concat ["(\\", x, ": ", show ty1, ". ", prettyprint t, ")"]
prettyprint (App t1 t2)   = concat ["(", prettyprint t1,  ") (", prettyprint t2, ")"]
prettyprint (If t1 t2 t3) = concat ["(if ", prettyprint t1, " then ", prettyprint t2, " else ", prettyprint t3, ")"]
prettyprint (T)           = concat ["True"]
prettyprint (F)           = concat ["False"]
prettyprint (Unit)        = concat ["Unit"]
prettyprint (ParseError e)= e



