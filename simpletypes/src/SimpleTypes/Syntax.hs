module SimpleTypes.Syntax where

import Control.Applicative
import SimpleTypes.Types 

data Info = Info 
  { row :: Int
  , col :: Int
  } deriving Show

instance PrettryPrinter Info where
  prettyprint (Info l c)  = concat ["[line: ", show l, ", col: ", show c, " ] "]

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

instance PrettryPrinter Term where
  prettyprint (Var _ x)       = concat [" ", x, " "] -- , ": ", prettyprint ty]
  prettyprint (VarI _ x k)    = concat [x] --, ": ", prettyprint ty]
  prettyprint (Abs _ x ty1 t) = concat ["(\\ ", x, ": ", prettyprint ty1, ". ", prettyprint t, ")"]
  prettyprint (App _ t1 t2)   = concat ["(", prettyprint t1,  prettyprint t2, ")" ]
  prettyprint (Cond _ t1 t2 t3) = concat ["(cond ", paren t1, paren t2, paren t3, ")"]
  prettyprint (Let _ x t1 t2) = concat ["(let ", x, " = ", prettyprint t1, " in ", prettyprint t2, ")"]
  prettyprint (T _)           = concat [" true "]
  prettyprint (F _)           = concat [" false "]
  prettyprint (Unit _)        = concat [" unit" ]
  prettyprint (ParseError e)= e

paren t = "(" ++prettyprint t ++ ")"

