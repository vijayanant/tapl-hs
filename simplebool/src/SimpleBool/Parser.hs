module SimpleBool.Parser  where
  
import SimpleBool.Syntax
import SimpleBool.Types

import Data.Char
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

locInfo :: SourcePos -> Info
locInfo pos = Info (sourceLine pos) (sourceColumn pos) 

parens = between (string "(") (string ")")

parseProgram :: String -> Term
parseProgram prg = case parse parseApplication "Program" prg of
                      Left err -> ParseError (show err)
                      Right t -> t

parseApplication :: Parser Term
parseApplication = do
  l <- many1 parseExpression
  pos <- getPosition
  return $ foldl1 (App (locInfo pos)) l

parseTerm :: Parser Term
parseTerm = spaces >>
  (   try parseAbstraction
  <|> try parseTrue
  <|> try parseFalse
  <|> try parseIf
  <|> try parseVariable
  <|> try (parens parseTerm)
  ) <* spaces


parseExpression = do 
  spaces
  t <-  parseTerm
  spaces
  return t

parseTrue :: Parser Term
parseTrue = do 
  string "true"
  pos <- getPosition
  return $ T (locInfo pos)

parseFalse :: Parser Term
parseFalse = do 
  string "false"
  pos <- getPosition
  return $ F (locInfo pos)

parseVarLetter :: Parser Char
parseVarLetter = satisfy isLower 

-- variables
parseVariable = do
  v <- many1 parseVarLetter
  pos <- getPosition
  return $ Var (locInfo pos )v

-- lambda abstraction
parseAbstraction = do 
  char '\\'
  x <- many1 parseVarLetter 
  char ':'
  ty <- parseType
  char '.'
  t <- parseApplication
  pos <- getPosition
  return $ Abs (locInfo pos) x ty t 

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
  pos <- getPosition
  return $ If (locInfo pos) t1 t2 t3

