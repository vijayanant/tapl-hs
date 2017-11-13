module SimpleTypes.Parser.Parser  where
  
import Data.Maybe
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
locInfo pos = Info (sourceLine pos) (sourceColumn pos)  (sourceName pos)

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
          (   try  parseLiteral
          <|> try  parseAbstraction
          <|> try  parseCond
          <|> try  parseVariable
          <|> try  ( parens parseArithExpr )
          <|> try  ( parens parseSeq )
          <|> try  ( parens parseLet )
          <|> try  ( parens parseTerm )
          <|> try  parsePair
          <|> try  ( parens parseApplication )
          ) <* spaces

parseLiteral :: Parser Term
parseLiteral = try parseUnit
           <|> try parseFalse 
           <|> try parseTrue
           <|> try parseFloat
           <|> try parseInteger

binary s assoc = Ex.Infix (parseOp s) assoc

parseOp :: String -> Parser (Term -> Term -> Term)
parseOp op = do 
  pos <- getPosition
  reservedOp op
  return $ BinaryOp (locInfo pos) operation 
    where 
      operation = (fromJust (lookup op opTable))

opTable = [ ("+", Plus)
          , ("-", Minus)
          , ("*", Times)
          , ("/", Divide)
          ]
table = [
          [ binary "*" Ex.AssocLeft
          , binary "/" Ex.AssocLeft
          ]
        , [ binary "+" Ex.AssocLeft
          , binary "-" Ex.AssocLeft
          ]
        ]

parseArithExpr ::  Parser Term
parseArithExpr = do 
  pos <- getPosition
  Ex.buildExpressionParser table parseTerm

parseUnit :: Parser Term
parseUnit = do 
  reserved "unit"
  pos <- getPosition
  return $ Literal (locInfo pos) LUnit

parseTrue :: Parser Term
parseTrue = do 
  spaces >> reserved "true"
  pos <- getPosition
  return $ Literal (locInfo pos) (LBool True)

parseFalse :: Parser Term
parseFalse = do
  spaces >> reserved "false"
  pos <- getPosition
  return $ Literal (locInfo pos) (LBool False)

parseInteger :: Parser Term
parseInteger = do
  pos <- getPosition 
  i <-  T.integer lexer
  return $ Literal (locInfo pos) (LInt i)

parseFloat = do 
  pos <- getPosition 
  sign <- option 1 parseSign
  x <- T.float lexer
  return $ Literal (locInfo pos) (LFloat (sign * x))

parseSign = do 
  s <- oneOf "+-"
  return $ if s == '-' then (-1.0) else (1.0)

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

parseSeq :: Parser Term
parseSeq = do
  pos <- getPosition
  Ex.buildExpressionParser (seqOp (locInfo pos)) parseTerm

seqOp l = [[Ex.Infix (reservedOp ";" >> return (mkSeq l))  Ex.AssocRight]]

mkSeq loc a b = App loc (Abs loc  "_" TUnit b) a

parseLet :: Parser Term
parseLet = do 
  spaces 
  reserved "let"
  spaces
  x <- many1 parseVarLetter
  spaces >> reserved "="
  t1 <- parseTerm
  spaces 
  reserved "in"
  spaces 
  t2 <- parseTerm
  spaces 
  pos <- getPosition
  return $ Let (locInfo pos) x t1 t2

parsePair :: Parser Term
parsePair = do 
  string "{"
  t1 <- parseTerm
  string ","
  t2 <- parseTerm
  string "}"
  pos <- getPosition
  return $ Pair (locInfo pos) t1 t2

parseType:: Parser Type
parseType = try parseTypeUnit 
        <|> try parseTypeBool
        <|> try parseTypeInt
        <|> try parseTypeFloat
        <|> try parseTypeArr
        <|> try parseTypePair
        <|> try (parens parseType)

parseTypeUnit :: Parser Type
parseTypeUnit = do
  reserved "Unit" 
  return TUnit

parseTypeBool :: Parser Type
parseTypeBool = do
  reserved "Bool"
  return TBool

parseTypeInt :: Parser Type
parseTypeInt = do
  reserved "Int"
  return TInt

parseTypeFloat :: Parser Type
parseTypeFloat = do
  reserved "Float"
  return TFloat

parseTypePair :: Parser Type
parseTypePair = do
  string "{"
  t1 <- parseType
  string ","
  t2 <- parseType 
  string "}"
  return $ TPair t1 t2 

parseTypeArr = do
  spaces >> char '('
  spaces
  t1 <- parseType
  spaces >> string "->"
  spaces
  t2 <- parseType
  spaces >> char ')'
  return $ TArr t1 t2


