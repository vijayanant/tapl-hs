module SimpleTypes.Syntax where

import Control.Applicative
import SimpleTypes.Types 

data Info = Info 
  { row :: Int
  , col :: Int
  , file:: String
  } deriving Show

instance PrettryPrinter Info where
  prettyprint (Info l c f)  = concat ["[line: ", show l, ", col: ", show c, " ] "]

instance Eq Info where 
  (==) i1 i2 = True

data Lit =  LUnit 
          | LBool Bool
          | LInt Integer
          | LFloat Double
          deriving (Eq, Show)

instance PrettryPrinter Lit where
  prettyprint (LUnit)                = concat [" unit"]
  prettyprint (LBool True)           = concat [" true "]
  prettyprint (LBool False)          = concat [" false "]
  prettyprint (LInt  i)  | i < 0     = concat ["(", show i, ")"]
                         | otherwise = concat [" ", show i, " "]
  prettyprint (LFloat f) | f < 0     = concat ["(", show f, ")"]
                         | otherwise = concat [" ", show f, " "]

data Term = Literal    Info Lit
          | Var        Info String
          | VarI       Info String Int
          | Cond       Info Term   Term Term
          | Abs        Info String Type Term
          | App        Info Term   Term
          | Let        Info String Term Term
          | Pair       Info Term   Term
          | BinaryOp   Info Op Term Term
          | ParseError String
          deriving (Eq, Show)

instance PrettryPrinter Term where
  prettyprint (Literal   _  l)        = prettyprint l
  prettyprint (Var       _  x)        = concat [" ", x, " "] 
  prettyprint (VarI      _  x  k)     = concat [x] 
  prettyprint (Abs       _  x  ty1 t) = concat ["(\\ ", x, ": ", prettyprint ty1, ". ", prettyprint t, ")"]
  prettyprint (App       _  t1 t2)    = concat ["(", prettyprint t1,  prettyprint t2, ")" ]
  prettyprint (Cond      _  t1 t2 t3) = concat ["(cond ", paren t1, paren t2, paren t3, ")"]
  prettyprint (Let       _  x  t1 t2) = concat ["(let ", x, " = ", prettyprint t1, " in ", prettyprint t2, ")"]
  prettyprint (Pair      _  t1 t2)    = concat ["{", prettyprint t1, ", ", prettyprint t2,  "}"]
  prettyprint (BinaryOp  _  op t1 t2) = concat ["(", prettyprint t1, prettyprint op , prettyprint t2, ")" ]
  prettyprint (ParseError e)= e

paren t = "(" ++ prettyprint t ++ ")"


data Op = Plus
        | Minus
        | Times
        | Divide
        deriving (Eq, Show)

instance PrettryPrinter Op where 
  prettyprint (Plus)   = " + " 
  prettyprint (Minus)  = " - " 
  prettyprint (Times)  = " * " 
  prettyprint (Divide) = " / " 

