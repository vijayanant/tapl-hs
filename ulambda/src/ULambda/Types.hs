module ULambda.Types where

import Control.Applicative

data Term = Var String 
          | VarI String Int  -- variable as de Bruijn index 
          | Abs String Term -- lambda abstraction
          | App Term Term   -- application
          | ParseError String

instance Show Term where 
  show (Var x)       = x
  show (VarI x k)    = x
  show (Abs x t)     = ("(\\" ++ x ++ "." ++ show t ++ ")")
  show (App t1 t2)   = show t1 ++ " " ++ show t2
  show (ParseError e)= show e
