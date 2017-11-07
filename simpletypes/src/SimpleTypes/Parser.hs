module SimpleTypes.Parser  where
  
import SimpleTypes.Syntax
import SimpleTypes.Types

import Data.Char
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

parseProgram :: String -> Term
parseProgram prg = case parse parseApplication "Program" prg of
                      Left err -> ParseError (show err)
                      Right t -> t

parseApplication :: Parser Term
parseApplication = ( foldl1 App ) <$> ( many1 parseTerm )

parseTerm :: Parser Term
parseTerm = spaces >>  (
  parseUnit  <|>
  parseTrue  <|>
  parseFalse <|>
  parseAbstraction <|> 
  parseCond    <|>
  parseVariable <|>
  {-parseSeq <|>-}
  parens parseTerm <|>
  parens parseApplication    
  ) <* spaces

parens :: Parser Term -> Parser Term
parens = between (string "(") (string ")") 

parseUnit :: Parser Term
parseUnit = string "unit" >> return Unit

parseTrue :: Parser Term
parseTrue = string "True" >> return T

parseFalse :: Parser Term
parseFalse = string "False" >> return F

parseVarLetter :: Parser Char
parseVarLetter = satisfy isLower 

-- variables
parseVariable :: Parser Term
parseVariable = do
  v <- many1 parseVarLetter
  return $ Var v

-- lambda abstraction
parseAbstraction :: Parser Term
parseAbstraction = do 
  char '\\'
  x <- many1 parseVarLetter 
  char ':'
  ty <- parseType
  char '.'
  t <- parseApplication
  return $ Abs x ty t 

{-parseE = parseTerm <|> parseApplication-}

parseE = parseApplication <|> parseTerm

parseCond :: Parser Term
parseCond = do 
  string "cond" 
  spaces
  t1 <- parens parseApplication 
  spaces
  t2 <- parens  parseApplication
  spaces
  t3 <- parseApplication
  return $ Cond t1 t2 t3

parseSeq :: Parser Term
parseSeq = do 
  char ';'
  t1 <- parseTerm 
  char ';'
  t2 <- parseTerm 
  return $ App (Abs "_" TUnit t2) t1


{-parseSeq :: Parser Term-}
{-parseSeq = do-}
  {-t1 <- parseApplication -}
  {-(parseSeq' >>= \t2 -> return $ App (Abs "_" TUnit t2) t1) <|> return t1-}

{-parseSeq' = do-}
  {-char ';'-}
  {-parseApplication-}

parseType = parseTypeUnit <|> 
            parseTypeBool <|> 
            parseTypeArr

parseTypeUnit = do
  string "Unit" 
  return TUnit

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


