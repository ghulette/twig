module Rewriting.Parser where

import Rewriting.Term
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

-- Parsing terms and rule literals

variable :: Parser Term
variable = do
  x <- upper
  xs <- many alphaNum
  spaces
  return $ Var (x:xs)

constant :: Parser Term
constant = do
  x <- lower
  xs <- many alphaNum
  spaces
  ts <- option [] termList
  return $ Const (x:xs) ts

term :: Parser Term
term = variable <|> constant <?> "term"

termList :: Parser [Term]
termList = between lparen rparen (term `sepBy` comma)
  where comma = char ',' >> spaces
        lparen = char '(' >> spaces
        rparen = char ')' >> spaces

rule :: Parser Rule
rule = do 
  t1 <- term
  rightArrow
  t2 <- term
  return $ Rule t1 t2
  where rightArrow = string "->" >> spaces


-- Parsing rules with combinators

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
ruleId = do
  x <- lower
  xs <- many alphaNum
  spaces
  return (x:xs)

ruleVar :: Parser RuleExpr
ruleVar = do
  x <- ruleId
  return (RuleVar x)

ruleLit :: Parser RuleExpr
ruleLit = do
  r <- between lbrace rbrace rule
  return (RuleLit r)
  where lbrace = char '{' >> spaces
        rbrace = char '}' >> spaces

ruleSuccess :: Parser RuleExpr
ruleSuccess = do
  _ <- char 'T'
  spaces
  return Success

ruleFailure :: Parser RuleExpr
ruleFailure = do
  _ <- char 'F'
  spaces
  return Failure

prefixOp :: Char -> (a -> a) -> Operator Char st a 
prefixOp c f = Prefix (char c >> spaces >> return f)

infixOp :: Char -> (a -> a -> a) -> Assoc -> Operator Char st a 
infixOp c f assoc = Infix (char c >> spaces >> return f) assoc

ruleExpr :: Parser RuleExpr
ruleExpr = buildExpressionParser table factor
  where table = [[prefixOp '?' Test,prefixOp '~' Neg],
                 [infixOp ';' Seq AssocLeft],
                 [infixOp '|' Choice AssocLeft]]
        factor =  between lparen rparen ruleExpr
              <|> ruleVar
              <|> ruleLit 
              <|> ruleSuccess 
              <|> ruleFailure
              <?> "factor"
        lparen = char '(' >> spaces
        rparen = char ')' >> spaces

ruleAssign :: Parser (String,RuleExpr)
ruleAssign = do
  x <- ruleId
  equals
  r <- ruleExpr
  return (x,r)
  where equals = char '=' >> spaces

ruleAssigns :: Parser [(String,RuleExpr)]
ruleAssigns = do
  spaces
  rs <- many ruleAssign
  eof
  return rs


-- Wrappers

parseRules :: String -> Either ParseError [(String,RuleExpr)]
parseRules = parse ruleAssigns "Rules"

parseTerm :: String -> Either ParseError Term
parseTerm = parse term "Term"
