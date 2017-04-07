module Parser (runParse, parseModule) where

import           AbsSyn

import           Control.Monad
import           Data.Char
import           Text.Parsec
import           Text.Parsec.Language
import qualified Text.Parsec.Token    as P

type Parse a = Parsec String Int a
runParse :: Parse a -> String -> a
runParse p inp =
  case runParser (whiteSpace >> p) 1 "inp" inp of
    Left err -> error (show err)
    Right v  -> v

checkIndent :: Parse ()
checkIndent = do
  pos <- getPosition
  req <- getState
  when (sourceColumn pos < req) $
    unexpected "indentation"

block :: Parse a -> Parse a
block p = do
  whiteSpace
  pos <- getPosition
  i <- getState
  setState (sourceColumn pos)
  v <- p
  setState i
  return v

indent :: Parse a -> Parse a
indent p = do
  i <- getState
  setState (i+1)
  v <- p
  setState i
  return v

lexer = P.makeTokenParser haskellDef
identifier = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
parens = P.parens lexer
symbol = P.symbol lexer
stringLiteral = P.stringLiteral lexer
charLiteral = P.charLiteral lexer
integer = P.integer lexer
float = P.float lexer
whiteSpace = P.whiteSpace lexer

parseUpperName :: Parse String
parseUpperName = try $ do
  name <- identifier
  unless (isUpper $ head name) $ fail "Wanted upper case letter"
  return name

parseLowerName :: Parse String
parseLowerName = try $ do
  name <- identifier
  unless (isLower $ head name) $ fail "Wanted upper case letter"
  return name

parseModule :: Parse Module
parseModule = do
  ds <- many parseDataDecl
  fs <- many parseFunDecl
  return $ Module ds fs

parseDataDecl :: Parse DataDecl
parseDataDecl = do
  reserved "data"
  name <- parseUpperName
  tyArgs <- many parseLowerName
  reservedOp "="
  cons <- indent (parseDataConstructor `sepBy1` reservedOp "|")
  return $ DataDecl name tyArgs cons

parseDataConstructor :: Parse DataConstructor
parseDataConstructor = checkIndent >> do
  con <- parseUpperName
  fields <- many parseAType
  return $ DataConstructor con fields
  <?> "data constructor"

parseTyVar :: Parse TyVar
parseTyVar = parseLowerName


parseType :: Parse Type
parseType =
  fold <$> parseBType
       <*> many (symbol "->" *> parseBType)
  where
    fold t [] = t
    fold t (x:xs) = TyFun t (fold x xs)


parseBType :: Parse Type
parseBType = foldl TyApp <$> parseAType <*> many parseAType

parseAType :: Parse Type
parseAType = checkIndent >> msum
  [ TyVar <$> parseLowerName
  , TyCon <$> parseUpperName
  , parens parseType
  ]


parseFunDecl :: Parse FunDecl
parseFunDecl = do
  fnName <- parseLowerName
  reservedOp ":"
  fnType <- indent parseType
  symbol fnName
  args <- many parseLowerName
  reservedOp "="
  body <- indent parseExpr
  return $ FunDecl fnName fnType args body

parseExpr :: Parse Expr
parseExpr = checkIndent >> msum
  [ parseApp
  , parseLet
  , parseLambda
  , parseCase ]
  <?> "expression"

parseApp :: Parse Expr
parseApp = do
  e <- parseSingleExpr
  es <- many parseSingleExpr
  return $ foldl App e es

parseLambda :: Parse Expr
parseLambda = do
  reservedOp "\\"
  var <- parseLowerName
  reservedOp "->"
  body <- parseExpr
  return $ Lam var body
  <?> "lambda"

parseSingleExpr :: Parse Expr
parseSingleExpr = checkIndent >> msum
  [ parseLitChar
  , try parseLitFloat
  , parseLitInt
  , parseLitString
  , parseXmlNode
  , parens parseExpr
  , parseVariable
  , parseConstructor ]

parseVariable :: Parse Expr
parseVariable = Var <$> parseLowerName

parseConstructor :: Parse Expr
parseConstructor = Con <$> parseUpperName

parseLitChar :: Parse Expr
parseLitChar = LitChar <$> charLiteral

parseLitString :: Parse Expr
parseLitString = (LitString . parseFvgString) <$> stringLiteral

parseLitInt :: Parse Expr
parseLitInt = LitInt <$> integer

parseLitFloat :: Parse Expr
parseLitFloat = LitFloat <$> float

parseXmlNode :: Parse Expr
parseXmlNode = block $ do
  reservedOp "<"
  node <- identifier
  props <- many $ do
    key <- identifier <|> stringLiteral
    reservedOp "="
    value <- stringLiteral
    return (key, parseFvgString value)
  reservedOp ">"
  children <- indent (parseExpr `sepBy` reservedOp ";")
  checkIndent
  reservedOp "</"
  symbol node
  reservedOp ">"
  return $ XmlNode node props children

parseLet :: Parse Expr
parseLet = do
  reserved "let"
  lhs <- parseLowerName
  reservedOp "="
  rhs <- parseExpr
  reserved "in"
  body <- parseExpr
  return $ Let lhs rhs body
 <?> "let"

parseCase :: Parse Expr
parseCase = block $ do
  reserved "case"
  scrut <- parseExpr
  reserved "of"
  alts <- indent $ many1 (checkIndent >> parseAlt)
  return $ Case scrut alts

-- pattern -> expr
parseAlt :: Parse Alt
parseAlt = block $ do
  pattern <- parsePattern
  symbol "->"
  expr <- indent $ parseExpr
  return $ Alt pattern expr

-- 10
-- 'c'
-- Node a b c
parsePattern :: Parse Pattern
parsePattern = checkIndent >> msum
  [ PatternChar <$> charLiteral
  , PatternInt <$> integer
  , PatternNode <$> parseUpperName <*> many parseLowerName ]

parseFvgString :: String -> FvgString
parseFvgString = merge . findStartMark
  where
    merge (TextBlock a:TextBlock b:xs)= merge (TextBlock (a++b):xs)
    merge (x:xs) = x:merge xs
    merge [] = []
    findStartMark ('{':'{':started) =
      let (var, rest) = findEndMark "" started
      in VariableBlock var : parseFvgString rest
    findStartMark (x:xs) = TextBlock [x] : findStartMark xs
    findStartMark [] = []
    findEndMark acc ('}':'}':xs) = (reverse acc,xs)
    findEndMark acc (x:xs) = findEndMark (x:acc) xs
    findEndMark acc [] = (reverse acc, [])
