module Arith.Syntax where 

data Term = TTrue
          | TFalse
          | TIf Term Term Term
          | TZero
          | TSucc Term
          | TPred Term
          | TIsZero Term
          deriving (Show)

