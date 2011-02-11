module Term
( Term (..)
, fromString
, children
, withChildren
, isLeaf
) where

import Data.List (intercalate)
import Rewriting.Lexer
import Text.ParserCombinators.Parsec

-- Terms

data Term = Term String [Term] 
  deriving Eq

instance Show Term where
  show (Term k []) = k
  show (Term k ts) = k ++ "(" ++ (intercalate "," (map show ts)) ++ ")"

children :: Term -> [Term]
children (Term _ ts) = ts

withChildren :: Term -> [Term] -> Term
withChildren (Term x _) ts = Term x ts

isLeaf :: Term -> Bool
isLeaf (Term _ []) = True
isLeaf (Term _ _) = False

fromString :: String -> Term
fromString s = 
  case parseTerm s of
    Left _ -> error "Not a term"
    Right t -> t

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
  return $ Term x ts

parseTerm :: String -> Either ParseError Term
parseTerm = parse (allOf term) "Term"
