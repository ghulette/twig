module Parser 
( parseStrategy
, parseTerm
) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Expr as Ex
import Lexer
import Term
import Pattern
import Expr

-- Identifiers

constId :: Parser String
constId = lexeme $ do 
  x <- lower
  xs <- many alphaNum
  return (x:xs)

varId :: Parser String
varId = lexeme $ do
  x <- upper
  xs <- many alphaNum
  return (x:xs)


-- Term patterns and rules

variable :: Parser Pattern
variable = do
  x <- varId
  return $ Var x

constant :: Parser Pattern
constant = do
  x <- constId
  ts <- option [] $ parens (pattern `sepBy` comma)
  return $ Const x ts

pattern :: Parser Pattern
pattern = variable <|> constant <?> "pattern"

rule :: Parser Expr
rule = do 
  t1 <- pattern
  reservedOp "->"
  t2 <- pattern
  return $ Rule t1 t2

-- Terms (no variables)

term :: Parser Term
term = do
  x <- constId
  ts <- option [] $ parens (term `sepBy` comma)
  return $ Term x ts

-- Rule expressions

ruleLit :: Parser Expr
ruleLit = do
  r <- brackets rule
  return $ r

success :: Parser Expr
success = do
  reserved "T"
  return Success

failure :: Parser Expr
failure = do
  reserved "F"
  return Failure

strategyExpr :: Parser Expr
strategyExpr = Ex.buildExpressionParser table factor
  where prefixOp x f = Ex.Prefix (reservedOp x >> return f)
        infixOp x f = Ex.Infix (reservedOp x >> return f)
        table = [[prefixOp "?" Test,prefixOp "~" Neg],
                 [infixOp ";" Seq Ex.AssocLeft],
                 [infixOp "|" Choice Ex.AssocLeft]
                ]
        factor =  parens strategyExpr
              <|> ruleLit
              <|> success 
              <|> failure
              <?> "factor"

-- Wrappers

parseStrategy :: String -> Either ParseError Expr
parseStrategy = parse (allOf strategyExpr) "strategy"

parseTerm :: String -> Either ParseError Term
parseTerm = parse (allOf term) "term"
