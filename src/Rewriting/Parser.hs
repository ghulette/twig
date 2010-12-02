module Rewriting.Parser where

import Rewriting.Term
import Text.ParserCombinators.Parsec

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

rules :: Parser [Rule]
rules = do
  spaces
  rs <- many rule
  eof
  return rs

parseRules :: String -> Either ParseError [Rule]
parseRules = parse rules "Rules"

parseTerm :: String -> Either ParseError Term
parseTerm = parse term "Term"
