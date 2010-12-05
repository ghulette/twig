module Rewriting.Parser where

import Rewriting.Term
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language (haskellStyle)

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops = [";","|","?","~","->","="]
        ids = ["T","F"]
        style = haskellStyle {Tok.reservedOpNames = ops,
                              Tok.reservedNames = ids}

whiteSpace = Tok.whiteSpace lexer
lexeme     = Tok.lexeme lexer
symbol     = Tok.symbol lexer
natural    = Tok.natural lexer
parens     = Tok.parens lexer
brackets   = Tok.brackets lexer
semi       = Tok.semi lexer
comma      = Tok.comma lexer
identifier = Tok.identifier lexer
reserved   = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer


-- Parsing terms and rule literals

variable :: Parser Term
variable = lexeme $ do
  x <- upper
  xs <- many alphaNum
  return $ Var (x:xs)

constant :: Parser Term
constant = do
  x <- constId
  ts <- option [] termList
  return $ Const x ts
  where constId = lexeme $ do x <- lower
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


-- Parsing rule expressions

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

ruleVar :: Parser RuleExpr
ruleVar = do
  x <- identifier
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
  x <- identifier
  reservedOp "="
  r <- ruleExpr
  return (x,r)

allOf :: GenParser Char () a -> GenParser Char () a
allOf p = do
  whiteSpace
  r <- p
  eof
  return r


-- Wrappers

parseRules :: String -> Either ParseError [(String,RuleExpr)]
parseRules = parse (allOf (many ruleAssign)) "Rules"

parseRule :: String -> Either ParseError Rule
parseRule = parse (allOf rule) "Rule"

parseTerm :: String -> Either ParseError Term
parseTerm = parse term "Term"
