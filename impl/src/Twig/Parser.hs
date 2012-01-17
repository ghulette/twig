module Twig.Parser 
( parseAST
, parseTerms
) where

import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Twig.Lexer
import Twig.AST
import Twig.Pattern
import Twig.Term
import Twig.RuleExpr

type TwigParser a = Parsec [Char] () a

-- Term patterns

termId :: TwigParser Id
termId = lexeme $ do 
  x <- lower
  xs <- many alphaNum
  return (x:xs)

variable :: TwigParser Pattern
variable = lexeme $ do
  x <- upper
  xs <- many alphaNum
  return $ Var (x:xs)

constant :: TwigParser Pattern
constant = do
  x <- termId
  ts <- option [] $ parens (termPattern `sepBy` comma)
  return (Const x ts)

tuplePattern :: TwigParser Pattern
tuplePattern = do
  ts <- parens (termPattern `sepBy1` comma)
  return (Const tupleConstructor ts)

termPattern :: TwigParser Pattern
termPattern = tuplePattern <|> variable <|> constant <?> "term pattern"

block :: TwigParser String
block = do
  reservedOp "<<<"
  m <- many (noneOf "<>") -- Fix this
  reservedOp ">>>"
  return m


-- Terms (no variables)

basicTerm :: TwigParser Term
basicTerm = do
  x <- termId
  ts <- option [] $ parens (term `sepBy` comma)
  return $ Term x ts

tupleTerm :: TwigParser Term
tupleTerm = do
  ts <- parens (term `sepBy1` comma)
  return $ Term tupleConstructor ts

term :: TwigParser Term
term = tupleTerm <|> basicTerm <?> "term"


-- Rule expressions

ruleId :: TwigParser Id
ruleId = identifier

var :: TwigParser RuleExpr
var = do
  x <- ruleId
  return (VarExpr x)

call :: TwigParser RuleExpr
call = do
  x <- ruleId
  args <- parens (ruleExpr `sepBy1` comma)
  return (Call x args)

ruleLit :: TwigParser (Pattern,Pattern)
ruleLit = brackets $ do
  lhs <- termPattern
  reservedOp "->"
  rhs <- termPattern
  return (lhs,rhs)

primRule :: TwigParser RuleExpr
primRule = do
  (lhs,rhs) <- ruleLit
  m <- option "" block
  return (Rule lhs rhs m)

success :: TwigParser RuleExpr
success = do
  reserved "T"
  return Success

failure :: TwigParser RuleExpr
failure = do
  reserved "F"
  return Failure

fix :: TwigParser RuleExpr
fix = do
  reserved "fix"
  parens $ do
    x <- ruleId
    comma
    e <- ruleExpr
    return (Fix x e)

branch :: TwigParser (RuleExpr -> RuleExpr)
branch = do
  reservedOp "#"
  branchOp
  where branchOp =  (reserved "all"  >> return BranchAll)
                <|> (reserved "some" >> return BranchSome)
                <|> (reserved "one"  >> return BranchOne)
                <|> (natural >>= \i  -> return (Path i))
                <?> "branch operator"

congruence :: TwigParser RuleExpr
congruence = do
  xs <- braces (ruleExpr `sepBy` comma)
  return (Congruence xs)

ruleExpr :: TwigParser RuleExpr
ruleExpr = Ex.buildExpressionParser table factor
  where prefixOp x f = Ex.Prefix (reservedOp x >> return f)
        infixOp  x f = Ex.Infix  (reservedOp x >> return f)
        table = [[prefixOp "?" Test,prefixOp "~" Neg],
                 [Ex.Prefix branch],
                 [infixOp ";" Seq Ex.AssocLeft],
                 [infixOp "|" LeftChoice Ex.AssocLeft]]
        factor =  parens ruleExpr
              <|> try call
              <|> var
              <|> primRule
              <|> fix
              <|> success 
              <|> failure
              <|> congruence
              <?> "factor"

-- AST

ruleStmt :: TwigParser Stmt
ruleStmt = do
  x <- ruleId
  reservedOp "="
  e <- ruleExpr
  return (RuleStmt x e)

defStmt :: TwigParser Stmt
defStmt = do
  x <- ruleId
  params <- parens (ruleId `sepBy1` comma)
  reservedOp "="
  e <- ruleExpr
  return (DefStmt x params e)

stmt :: TwigParser Stmt
stmt = try defStmt <|> ruleStmt <?> "Statement"

unit :: TwigParser Unit
unit = do
  stmts <- many stmt
  return (Unit stmts)

ast :: TwigParser Top
ast = do
  u <- unit
  return (Top [u])

-- Wrappers

parseAST :: String -> Either ParseError Top
parseAST = parse (allOf ast) "Twig"

parseTerms :: String -> Either ParseError [Term]
parseTerms = parse (allOf (many term)) "Terms"
