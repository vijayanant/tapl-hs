module SimpleBool.Parser  where
  
import SimpleBool.Syntax
import SimpleBool.Types

import Data.Char
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

parseProgram :: String -> Term
parseProgram prg = case parse parseApplication "Program" prg of
                      Left err -> ParseError (show err)
                      Right t -> t

parseApplication :: Parser Term
parseApplication = do
  l <- many1 parseExpression
  return $ foldl1 App l

parseTerm :: Parser Term
parseTerm = spaces >> (
  parseAbstraction <|> 
  parseTrue  <|>
  parseFalse <|>
  parseIf    <|>
  parseVariable <|>
  between (string "(") (string ")") parseTerm
  )

parseExpression = do 
  spaces
  t <-  parseTerm
  spaces
  return t

parseTrue :: Parser Term
parseTrue = string "true" >> return T

parseFalse :: Parser Term
parseFalse = string "false" >> return F

parseVarLetter :: Parser Char
parseVarLetter = satisfy isLower 

-- variables
parseVariable = do
  v <- many1 parseVarLetter
  return $ Var v

-- lambda abstraction
parseAbstraction = do 
  char '\\'
  x <- many1 parseVarLetter 
  char ':'
  ty <- parseType
  char '.'
  t <- parseApplication
  return $ Abs x ty t 

parseType = parseTypeBool <|> parseTypeArr

parseTypeBool = do
  string "Bool"
  return TBool

parseTypeArr = do 
  spaces >> char '('
  spaces 
  t1 <- parseType 
  spaces >> string "->"
  spaces 
  t2 <- parseType
  spaces >> char ')'
  return $ TArr t2 t2


parseIf = do 
  string "if" 
  spaces
  t1 <- parseTerm 
  spaces
  string "then"
  spaces
  t2 <-  parseTerm
  spaces
  string "else"
  spaces
  t3 <- parseTerm
  spaces
  return $ If t1 t2 t3

