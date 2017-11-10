module SimpleTypes.Parser.Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as T

lexer :: T.TokenParser ()
lexer = T.makeTokenParser style
  where
    names = ["unit", "Unit", "Bool", "True", "False"]
    ops   = [ ";", ":", "."]
    style = emptyDef 
             { T.commentLine = "--"
             , T.reservedOpNames = ops
             , T.reservedNames = names 
             }

parens :: Parser a -> Parser a
parens = T.parens lexer

semiSep :: Parser a -> Parser [a]
semiSep = T.semiSep lexer

identifier :: Parser String
identifier = T.identifier lexer

reserved :: String -> Parser ()
reserved = T.reserved lexer

whitespace :: Parser ()
whitespace = T.whiteSpace lexer

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer
