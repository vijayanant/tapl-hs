module ULambda.Parser (
  parseProgram
) where
  
import ULambda.Types
import ULambda.Evaluator

import Data.Char
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

parseProgram :: String -> Term
parseProgram prg = case parse parseApplication "code" prg of
                      Left err -> ParseError (show err)
                      Right t -> t

parseTerm :: Parser Term
parseTerm = spaces >> (
  parseVariable     <|>
  parseExpression <|>
  parseAbstraction  
  )

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
  char '.'
  y <- parseApplication
  return $ Abs x y 

parseExpression = do 
  char '('
  t <-  parseApplication
  char ')'
  return t

parseApplication = do
  l <- many1 parseTerm
  return $ foldl1 App l
