module SimpleBool.Syntax where

import Control.Applicative

import SimpleBool.Types

data Info = Info 
  { row :: Int
  , col :: Int
  }

instance Show Info where
  show (Info l c) = concat ["[line: ", show l, ", col: ", show c, " ] "]

data Term = Var Info  String 
          | VarI Info String Int
          | T Info
          | F Info
          | If Info Term Term Term
          | Abs Info String Type Term -- lambda abstraction with type
          | App Info Term Term
          | ParseError String

instance Show Term where 
  show (Var _  x)       = concat [x] -- , ": ", show ty]
  show (VarI _ x k)    = concat [x] --, ": ", show ty]
  show (Abs _ x ty1 t) = concat ["(\\", x, ": ", show ty1, ". ", show t, ")"]
  show (App _ t1 t2)   = concat ["(", show t1,  ") (", show t2, ")"]
  show (If _ t1 t2 t3) = concat ["(if ", show t1, " then ", show t2, " else ", show t3, ")"]
  show (T _)           = concat ["True"]
  show (F _)           = concat ["False"]
  show (ParseError e)= e



