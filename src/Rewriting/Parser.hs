module Rewriting.Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Rewriting.Term
import Rewriting.Lexer

-- Terms and rule literals

variable :: Parser Term
variable = lexeme $ do
  x <- upper
  xs <- many alphaNum
  return $ Var (x:xs)

constant :: Parser Term
constant = do
  x <- lexeme constId
  ts <- option [] termList
  return $ Const x ts
  where constId = do x <- lower
                     xs <- many alphaNum
                     return (x:xs)

term :: Parser Term
term = variable <|> constant <?> "term"

termList :: Parser [Term]
termList = parens (term `sepBy` comma)

rule :: Parser Rule
rule = do 
  t1 <- term
  reservedOp "->"
  t2 <- term
  return $ Rule t1 t2


-- Expressions

data RuleExpr = RuleVar String
              | RuleLit Rule
              | Success
              | Failure
              | Test RuleExpr
              | Neg RuleExpr
              | Seq RuleExpr RuleExpr
              | Choice RuleExpr RuleExpr
              -- | BranchAll RuleExpr
              -- | BranchOne RuleExpr
              -- | BranchSome RuleExpr
              deriving (Eq,Show)

ruleId :: Parser String
ruleId = identifier

ruleVar :: Parser RuleExpr
ruleVar = do
  x <- ruleId
  return (RuleVar x)

ruleLit :: Parser RuleExpr
ruleLit = do
  r <- brackets rule
  return (RuleLit r)

ruleSuccess :: Parser RuleExpr
ruleSuccess = do
  reserved "T"
  return Success

ruleFailure :: Parser RuleExpr
ruleFailure = do
  reserved "F"
  return Failure

ruleExpr :: Parser RuleExpr
ruleExpr = buildExpressionParser table factor
  where prefixOp x f = Prefix (reservedOp x >> return f)
        infixOp x f = Infix (reservedOp x >> return f)
        table = [[prefixOp "?" Test,prefixOp "~" Neg],
                 [infixOp ";" Seq AssocLeft],
                 [infixOp "|" Choice AssocLeft]]
        factor =  parens ruleExpr
              <|> ruleVar
              <|> ruleLit 
              <|> ruleSuccess 
              <|> ruleFailure
              <?> "factor"

ruleAssign :: Parser (String,RuleExpr)
ruleAssign = do
  x <- ruleId
  reservedOp "="
  r <- ruleExpr
  return (x,r)


-- Wrappers

parseRules :: String -> Either ParseError [(String,RuleExpr)]
parseRules = parse (allOf (many ruleAssign)) "Rules"

parseTerm :: String -> Either ParseError Term
parseTerm = parse term "Term"
