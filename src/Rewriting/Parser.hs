module Rewriting.Parser 
(RuleExpr(..)
,RuleStmt(..)
,parseRules
,parseTerms
)where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Rewriting.Term
import Rewriting.Lexer

type Id = String

-- Terms and rule literals

constId :: Parser Id
constId = do 
  x <- lower
  xs <- many alphaNum
  return (x:xs)

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


-- ConstTerms (no variables)

constTerm :: Parser Term
constTerm = do
  x <- lexeme constId
  ts <- option [] constTermList
  return $ Const x ts

constTermList :: Parser [Term]
constTermList = parens (constTerm `sepBy` comma)


-- Expressions

data RuleExpr = RuleVar Id
              | RuleLit Rule
              | Macro Id [RuleExpr]
              | Success
              | Failure
              | Test RuleExpr
              | Neg RuleExpr
              | Seq RuleExpr RuleExpr
              | Choice RuleExpr RuleExpr
              | BranchAll RuleExpr
              | BranchOne RuleExpr
              | BranchSome RuleExpr
              | Congruence [RuleExpr]
              | Path Integer
              | Root Id
              deriving (Eq,Show)

data RuleStmt = RuleDef Id RuleExpr 
              | MacroDef Id [Id] RuleExpr
              deriving (Eq,Show)

ruleId :: Parser Id
ruleId = identifier

ruleVar :: Parser RuleExpr
ruleVar = do
  x <- ruleId
  return (RuleVar x)

macro :: Parser RuleExpr
macro = do
  x <- ruleId
  args <- parens (ruleExpr `sepBy1` comma)
  return (Macro x args)

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

rulePath :: Parser RuleExpr
rulePath = do
  reservedOp "#"
  i <- natural
  return (Path i)

ruleRoot :: Parser RuleExpr
ruleRoot = do
  x <- brackets ruleId
  return (Root x)

ruleCongruence :: Parser RuleExpr
ruleCongruence = do
  xs <- braces (ruleExpr `sepBy` comma)
  return (Congruence xs)

ruleExpr :: Parser RuleExpr
ruleExpr = buildExpressionParser table factor
  where prefixOp x f = Prefix (reservedOp x >> return f)
        infixOp x f = Infix (reservedOp x >> return f)
        table = [[prefixOp "?" Test,prefixOp "~" Neg,
                  prefixOp "one" BranchOne,
                  prefixOp "some" BranchSome,
                  prefixOp "all" BranchAll],
                 [infixOp ";" Seq AssocLeft],
                 [infixOp "|" Choice AssocLeft]]
        factor =  parens ruleExpr
              <|> try macro
              <|> ruleVar
              <|> try ruleLit
              <|> try ruleRoot
              <|> rulePath
              <|> ruleSuccess 
              <|> ruleFailure
              <|> ruleCongruence
              <?> "factor"

ruleOrMacroDef :: Id -> [Id] -> RuleExpr -> RuleStmt
ruleOrMacroDef x [] r = RuleDef x r
ruleOrMacroDef x ps r = MacroDef x ps r

ruleStmt :: Parser RuleStmt
ruleStmt = do
  x <- ruleId
  params <- option [] (parens (ruleId `sepBy1` comma))
  reservedOp "="
  r <- ruleExpr
  return (ruleOrMacroDef x params r)

ruleStmts :: Parser [RuleStmt]
ruleStmts = do
  ds <- many ruleStmt
  return (reverse ds)

-- Wrappers

parseRules :: String -> Either ParseError [RuleStmt]
parseRules = parse (allOf ruleStmts) "Rules"

parseTerms :: String -> Either ParseError [Term]
parseTerms = parse (allOf (many constTerm)) "Terms"
