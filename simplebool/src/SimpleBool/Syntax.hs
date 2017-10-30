module SimpleBool.Syntax where

import Control.Applicative

import SimpleBool.Types

data Term = Var String 
          | VarI String Int
          | T
          | F
          | If Term Term Term
          | Abs String Type Term -- lambda abstraction with type
          | App Term Term
          | ParseError String
          {-deriving Show-}

instance Show Term where 
  show (Var x)       = concat [x] -- , ": ", show ty]
  show (VarI x k)    = concat [x] --, ": ", show ty]
  show (Abs x ty1 t) = concat ["(\\", x, ": ", show ty1, ". ", show t, ")"]
  show (App t1 t2)   = concat ["(", show t1,  ") (", show t2, ")"]
  show (If t1 t2 t3) = concat ["(if ", show t1, " then ", show t2, " else ", show t3, ")"]
  show (T)           = concat ["True"]
  show (F)           = concat ["False"]
  show (ParseError e)= e



