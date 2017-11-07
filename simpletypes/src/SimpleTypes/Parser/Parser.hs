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

contentParser :: Parser Term -> Parser Term
contentParser p = do 
  T.whiteSpace lexer
  c <- p
  eof
  return c

parseProgram :: String -> Either ParseError Term
parseProgram prg = parse (contentParser parseApplication ) "Program" prg 

parseApplication :: Parser Term
parseApplication = ( foldl1 App ) <$> ( many1 parseTerm )

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
          <|> try  ( parens parseSeq )
          ) <* spaces

parseUnit :: Parser Term
parseUnit = reserved "unit" >> return Unit

parseTrue :: Parser Term
parseTrue = reserved "True" >> return T

parseFalse :: Parser Term
parseFalse = reserved "False" >> return F

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
  reservedOp "\\"
  x <- many1 parseVarLetter 
  reservedOp ":"
  ty <- parseType
  reservedOp "."
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
  spaces
  return $ Cond t1 t2 t3


binops = [[Ex.Infix (reservedOp ";" >> return mkSeq )  Ex.AssocLeft]]

parseSeq :: Parser Term
parseSeq =  Ex.buildExpressionParser binops parseTerm

mkSeq a b = App (Abs "_" TUnit b) a

parseType = parseTypeUnit <|> 
            parseTypeBool <|> 
            parseTypeArr

parseTypeUnit = do
  reserved "Unit" 
  return TUnit

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
  return $ TArr t2 t2


