module Rewriting.Parser 
( parseRules
, parseTerms
) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Expr as Ex
import Rewriting.Lexer
import Rewriting.Rule
import Rewriting.Term
import Rewriting.RuleExpr

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


-- Rule expressions

ruleId :: Parser Id
ruleId = identifier

ruleVar :: Parser RuleExpr
ruleVar = do
  x <- ruleId
  return (RuleVar x)

call :: Parser RuleExpr
call = do
  x <- ruleId
  args <- parens (ruleExpr `sepBy1` comma)
  return (Call x args)

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

rulePath :: Parser (RuleExpr -> RuleExpr)
rulePath = do
  reservedOp "#"
  i <- natural
  return (\s -> Path i s)

ruleCongruence :: Parser RuleExpr
ruleCongruence = do
  xs <- braces (ruleExpr `sepBy` comma)
  return (Congruence xs)

ruleExpr :: Parser RuleExpr
ruleExpr = Ex.buildExpressionParser table factor
  where prefixOp x f = Ex.Prefix (reservedOp x >> return f)
        infixOp x f = Ex.Infix (reservedOp x >> return f)
        table = [[prefixOp "?" Test,
                  prefixOp "~" Neg,
                  Ex.Prefix rulePath,
                  prefixOp "one" BranchOne,
                  prefixOp "some" BranchSome,
                  prefixOp "all" BranchAll ],
                 [infixOp ";" Seq Ex.AssocLeft ],
                 [infixOp "|" LeftChoice Ex.AssocLeft,
                  infixOp "+" Choice Ex.AssocLeft ]]
        factor =  parens ruleExpr
              <|> try call
              <|> ruleVar
              <|> ruleLit
              <|> ruleSuccess 
              <|> ruleFailure
              <|> ruleCongruence
              <?> "factor"


-- Rule definitions and constructors

ruleDef :: Parser RuleDef
ruleDef = do
  x <- ruleId
  params <- option [] (parens (ruleId `sepBy1` comma))
  reservedOp "="
  r <- ruleExpr
  return (RuleDef x params r)

ruleDefs :: Parser [RuleDef]
ruleDefs = do
  ds <- many ruleDef
  return (reverse ds)


-- Wrappers

parseRules :: String -> Either ParseError [RuleDef]
parseRules = parse (allOf ruleDefs) "Rules"

parseTerms :: String -> Either ParseError [Term]
parseTerms = parse (allOf (many constTerm)) "Terms"
