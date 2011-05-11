module Parser 
( parseAST
, parseTerms
) where

import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Lexer
import AST
import Pattern
import Term
import RuleExpr

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

trace :: TwigParser [String]
trace = do
  reservedOp ":"
  m <- stringLiteral
  return [m]


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

ruleLit :: TwigParser RuleExpr
ruleLit = brackets $ do
    lhs <- termPattern
    reservedOp "->"
    rhs <- termPattern
    m <- option [] trace
    return (Rule lhs rhs (return m))

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
              <|> ruleLit
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

invStmt :: TwigParser Stmt
invStmt = do
  reserved "inv"
  x1 <- ruleId
  x2 <- ruleId
  return (InvStmt x1 x2) 

stmt :: TwigParser Stmt
stmt = try defStmt <|> ruleStmt <|> invStmt <?> "Statement"

unit :: TwigParser Unit
unit = do
  stmts <- many stmt
  return (Unit stmts)

ast :: TwigParser Top
ast = do
  u <- unit
  return (Top [u])


-- Term inputs

application :: TwigParser (Id,Term)
application = do
  x <- ruleId
  t <- term
  return (x,t)

-- Wrappers

parseAST :: String -> Either ParseError Top
parseAST = parse (allOf ast) "Twig"

parseTerms :: String -> Either ParseError [(Id,Term)]
parseTerms = parse (allOf (many application)) "Terms"
