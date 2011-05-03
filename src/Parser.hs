module Parser 
( parseRules
, parseTerms
) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Expr as Ex
import Lexer
import Pattern (Pattern (..))
import Term
import RuleExpr
import Env (Env,fromList)

-- Term patterns and rule literals

termId :: Parser Id
termId = do 
  x <- lower
  xs <- many alphaNum
  return (x:xs)

variable :: Parser Pattern
variable = lexeme $ do
  x <- upper
  xs <- many alphaNum
  return $ Var (x:xs)

constant :: Parser Pattern
constant = do
  x <- lexeme termId
  ts <- option [] $ parens (termPattern `sepBy` comma)
  return $ Const x ts

tuplePattern :: Parser Pattern
tuplePattern = do
  ts <- angles (termPattern `sepBy1` comma)
  return $ Const tupleConstructor ts

termPattern :: Parser Pattern
termPattern = tuplePattern <|> variable <|> constant <?> "term pattern"

trace :: Parser [String]
trace = do
  reservedOp ":"
  m <- stringLiteral
  return [m]


-- Terms (no variables)

basicTerm :: Parser Term
basicTerm = do
  x <- lexeme termId
  ts <- option [] $ parens (term `sepBy` comma)
  return $ Term x ts

tupleTerm :: Parser Term
tupleTerm = do
  ts <- braces (term `sepBy1` comma)
  return $ Term tupleConstructor ts

term :: Parser Term
term = tupleTerm <|> basicTerm <?> "term"

-- Rule expressions

ruleId :: Parser Id
ruleId = identifier

var :: Parser RuleExpr
var = do
  x <- ruleId
  return (VarExpr x)

call :: Parser RuleExpr
call = do
  x <- ruleId
  args <- parens (ruleExpr `sepBy1` comma)
  return (Call x args)

ruleLit :: Parser RuleExpr
ruleLit = brackets $ do
    lhs <- termPattern
    reservedOp "->"
    rhs <- termPattern
    m <- option [] trace
    return (Rule lhs rhs (return m))

ruleSuccess :: Parser RuleExpr
ruleSuccess = do
  reserved "T"
  return Success

ruleFailure :: Parser RuleExpr
ruleFailure = do
  reserved "F"
  return Failure

ruleFix :: Parser RuleExpr
ruleFix = do
  reserved "Fix"
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
        prefixName x f = Ex.Prefix (reserved x >> return f)
        infixOp x f = Ex.Infix (reservedOp x >> return f)
        table = [[prefixOp "?" Test,
                  prefixOp "~" Neg,
                  Ex.Prefix rulePath,
                  prefixName "One" BranchOne,
                  prefixName "Some" BranchSome,
                  prefixName "All" BranchAll],
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

ruleDefs :: Parser (Env Proc)
ruleDefs = do
  procs <- many ruleProc
  return (Env.fromList procs)


-- Term inputs

application :: Parser (Id,Term)
application = do
  x <- ruleId
  t <- term
  return (x,t)

-- Wrappers

parseRules :: String -> Either ParseError (Env Proc)
parseRules = parse (allOf ruleDefs) "Rules"

parseTerms :: String -> Either ParseError [(Id,Term)]
parseTerms = parse (allOf (many application)) "Terms"
