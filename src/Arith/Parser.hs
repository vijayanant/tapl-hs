module Arith.Parser (
  parseTerm
) where
  
import Arith.Syntax
import Arith.Evaluator

import Text.Parsec
import Text.Parsec.String

parseTerm :: Parser Term
parseTerm = 
  parseTrue  <|>
  parseFalse <|>
  parseZero  <|>
  parseIf    <|>
  parseSucc  <|>
  parsePred  <|>
  parseIsZero<|>
  between (string "(") (string ")") parseTerm

parseTrue :: Parser Term
parseTrue = string "true" >> return TTrue

parseFalse :: Parser Term
parseFalse = string "false" >> return TFalse

parseZero :: Parser Term
parseZero = string "0" >> return TZero

parseIf :: Parser Term
parseIf = do 
  string "if" 
  spaces
  predicate <- parseTerm
  spaces 
  string "then"
  spaces 
  consequent <- parseTerm
  spaces
  string "else"
  spaces
  subsequent <- parseTerm
  return $ TIf predicate consequent subsequent

parseSucc :: Parser Term
parseSucc = string "succ" >> spaces >> parseTerm >>= return.TSucc

parsePred :: Parser Term
parsePred = string "pred" >> spaces >> parseTerm >>= return.TPred

parseIsZero :: Parser Term
parseIsZero = string "zero" >> spaces >> parseTerm >>= return.TIsZero
