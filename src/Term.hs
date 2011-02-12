module Term
  ( Term
  , readTerm
  , showTerm
  , isLeaf
  ) where

import Data.Tree
import Data.List (intercalate)
import Rewriting.Lexer
import Text.ParserCombinators.Parsec

-- Terms

type Term = Tree String

showTerm :: Term -> String
showTerm (Node k []) = k
showTerm (Node k ts) = k ++ "(" ++ (intercalate "," (map show ts)) ++ ")"

readTerm :: String -> Term
readTerm s = 
  case parseTerm s of
    Left _ -> error "Not a term"
    Right t -> t

isLeaf :: Term -> Bool
isLeaf (Node _ xs) = null xs

-- Parser functions

termId :: Parser String
termId = do 
  x <- lower
  xs <- many alphaNum
  return (x:xs)

termList :: Parser [Term]
termList = parens (term `sepBy` comma)

term :: Parser Term
term = do
  x <- lexeme termId
  ts <- option [] termList
  return $ Node x ts

parseTerm :: String -> Either ParseError Term
parseTerm = parse (allOf term) "Term"
