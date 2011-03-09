module Parser 
( parseRules
, parseTerms
) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Expr as Ex
import Lexer
import Rule (Rule (..))
import qualified Rule as Rule
import Term
import RuleExpr

-- Term patterns and rule literals

termId :: Parser Id
termId = do 
  x <- lower
  xs <- many alphaNum
  return (x:xs)

variable :: Parser Rule.TermPattern
variable = lexeme $ do
  x <- upper
  xs <- many alphaNum
  return $ Rule.Var (x:xs)

constant :: Parser Rule.TermPattern
constant = do
  x <- lexeme termId
  ts <- option [] $ parens (termPattern `sepBy` comma)
  return $ Rule.Const x ts

termPattern :: Parser Rule.TermPattern
termPattern = variable <|> constant <?> "term pattern"

rule :: Parser Rule
rule = do 
  t1 <- termPattern
  reservedOp "->"
  t2 <- termPattern
  return $ Rule t1 t2

trace :: Parser [String]
trace = do
  reservedOp ":"
  m <- stringLiteral
  return [m]


-- Terms (no variables)

term :: Parser Term
term = do
  x <- lexeme termId
  ts <- option [] $ parens (term `sepBy` comma)
  return $ Term x ts


-- Rule expressions

ruleId :: Parser Id
ruleId = identifier

var :: Parser RuleExpr
var = do
  x <- ruleId
  return (Var x)

call :: Parser RuleExpr
call = do
  x <- ruleId
  args <- parens (ruleExpr `sepBy1` comma)
  return (RuleCall x args)

ruleLit :: Parser RuleExpr
ruleLit = do
  (r,m) <- brackets $ do
    r <- rule
    m <- option [] trace
    return (r,m)
  return (RuleLit r m)

ruleSuccess :: Parser RuleExpr
ruleSuccess = do
  reservedOp "T"
  return Success

ruleFailure :: Parser RuleExpr
ruleFailure = do
  reservedOp "F"
  return Failure

ruleFix :: Parser RuleExpr
ruleFix = do
  reservedOp "Fix"
  parens $ do
    x <- ruleId
    comma
    e <- ruleExpr
    return (Fix x e)

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
                  prefixOp "One" BranchOne,
                  prefixOp "Some" BranchSome,
                  prefixOp "All" BranchAll],
                 [infixOp ";" Seq Ex.AssocLeft ],
                 [infixOp "|" LeftChoice Ex.AssocLeft,
                  infixOp "+" Choice Ex.AssocLeft ]]
        factor =  parens ruleExpr
              <|> try call
              <|> var
              <|> ruleLit
              <|> ruleFix
              <|> ruleSuccess 
              <|> ruleFailure
              <|> ruleCongruence
              <?> "factor"


-- Rule definitions and constructors

ruleProc :: Parser (Id,Proc)
ruleProc = do
  x <- ruleId
  params <- option [] (parens (ruleId `sepBy1` comma))
  reservedOp "="
  e <- ruleExpr
  return (x,Proc params e)

ruleDefs :: Parser RuleEnv
ruleDefs = do
  procs <- many ruleProc
  return (buildRuleEnv procs)


-- Wrappers

parseRules :: String -> Either ParseError RuleEnv
parseRules = parse (allOf ruleDefs) "Rules"

parseTerms :: String -> Either ParseError [Term]
parseTerms = parse (allOf (many term)) "Terms"
