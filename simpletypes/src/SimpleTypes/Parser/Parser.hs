module SimpleTypes.Parser.Parser  where
  
import Data.Char
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Expr as Ex

import SimpleTypes.Parser.Lexer
import SimpleTypes.Syntax
import SimpleTypes.Types

locInfo :: SourcePos -> Info
locInfo pos = Info (sourceLine pos) (sourceColumn pos) 

contentParser :: Parser Term -> Parser Term
contentParser p = do 
  T.whiteSpace lexer
  c <- p
  eof
  return c

parseProgram :: String -> Either ParseError Term
parseProgram prg = parse (contentParser parseApplication ) "Program" prg 

parseApplication :: Parser Term
parseApplication = do
  l <- many1 parseTerm
  pos <- getPosition
  return $ foldl1 (App (locInfo pos)) l

parseTerm :: Parser Term
parseTerm = spaces >> 
          (   try  parseUnit
          <|> try  parseTrue
          <|> try  parseFalse
          <|> try  parseAbstraction
          <|> try  parseCond
          <|> try  parseVariable
          <|> try  ( parens parseTerm )
          <|> try  ( parens parseApplication )
          {-<|> try  ( parseSeq )-}
          ) <* spaces

parseUnit :: Parser Term
parseUnit = spaces >> reserved "unit" >> 
            getPosition >>=
            \p -> return $ Unit (locInfo p)

parseTrue :: Parser Term
parseTrue = do 
  spaces >> reserved "true"
  pos <- getPosition
  return $ T (locInfo pos)

parseFalse :: Parser Term
parseFalse = do 
  spaces >> reserved "false"
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
  string "\\"
  spaces
  x <- many1 parseVarLetter 
  char ':'
  spaces
  ty <- parseType
  char '.'
  t <- parseApplication
  pos <- getPosition
  return $ Abs (locInfo pos) x ty t 

parseE = parseApplication <|> parseTerm

parseCond :: Parser Term
parseCond = do 
  reserved "cond" 
  spaces
  t1 <- parseTerm 
  spaces
  t2 <- parseTerm
  spaces
  t3 <- parseTerm
  spaces
  pos <- getPosition
  return $ Cond (locInfo pos) t1 t2 t3


binops l = [[Ex.Infix (reservedOp ";" >> return (mkSeq l))  Ex.AssocRight]]

parseSeq :: Parser Term
parseSeq = do
  pos <- getPosition
  Ex.buildExpressionParser (binops (locInfo pos)) parseTerm

mkSeq loc a b = App loc (Abs loc  "_" TUnit b) a

parseType:: Parser Type
parseType = try parseTypeUnit 
        <|> try parseTypeBool
        <|> try parseTypeArr
        <|> try (parens parseType)

parseTypeUnit :: Parser Type
parseTypeUnit = do
  reserved "Unit" 
  return TUnit

parseTypeBool :: Parser Type
parseTypeBool = do
  reserved "Bool"
  return TBool

parseTypeArr = do
  spaces >> char '('
  spaces
  t1 <- parseType
  spaces >> string "->"
  spaces
  t2 <- parseType
  spaces >> char ')'
  return $ TArr t1 t2


