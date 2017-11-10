module ULambda.Types where

import Control.Applicative

data Term = Var Info String 
          | VarI Info String Int  -- variable as de Bruijn index 
          | Abs Info String Term -- lambda abstraction
          | App Info Term Term   -- application
          | ParseError String

data Info = Info 
  { row :: Int
  , col :: Int
  } deriving Show

instance Show Term where 
  show (Var i x)       = x
  show (VarI i x k)    = x
  show (Abs i x t)     = ("(\\" ++ x ++ "." ++ show t ++ ")")
  show (App i t1 t2)   = show t1 ++ " " ++ show t2
  show (ParseError e)= show e
