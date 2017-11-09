module ULambda.Types where

import Control.Applicative

data Term = Var Loc String 
          | VarI Loc String Int  -- variable as de Bruijn index 
          | Abs Loc String Term -- lambda abstraction
          | App Loc Term Term   -- application
          | ParseError String

data Loc = Loc 
  { row :: Int
  , col :: Int
  } deriving Show

instance Show Term where 
  show (Var i x)       = x
  show (VarI i x k)    = x
  show (Abs i x t)     = ("(\\" ++ x ++ "." ++ show t ++ ")")
  show (App i t1 t2)   = show t1 ++ " " ++ show t2
  show (ParseError e)= show e
