module ULambda.Parser (
  parseProgram
) where
  
import ULambda.Types

import Data.Char
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

locInfo :: SourcePos -> Loc
locInfo pos = Loc (sourceLine pos) (sourceColumn pos) 

parseProgram :: String -> Term
parseProgram prg = case parse parseApplication "code" prg of
                      Left err -> ParseError (show err)
                      Right t -> t

parseApplication :: Parser Term
parseApplication = do
  l <- many1 parseTerm
  pos <- getPosition
  return $ foldl1 (App (locInfo pos)) l

parseTerm :: Parser Term
parseTerm = spaces >> 
  (   try parseVariable
  <|> try parseExpression
  <|> try parseAbstraction  
  )

parseVarLetter :: Parser Char
parseVarLetter = satisfy isLower 

-- variables
parseVariable = do
  v <- many1 parseVarLetter
  pos <- getPosition
  return $ Var (locInfo pos) v

-- lambda abstraction
parseAbstraction = do 
  char '\\'
  x <- many1 parseVarLetter 
  char '.'
  y <- parseApplication
  pos <- getPosition
  return $ Abs (locInfo pos) x y 

parseExpression = do 
  char '('
  t <-  parseApplication
  char ')'
  return t

