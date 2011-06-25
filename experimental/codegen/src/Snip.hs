{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Snip (Snip(..),parseSnip) where

import Text.Parsec
import Text.Parsec.String

data Snip = Snip String | InVar Int | OutVar Int deriving (Eq,Show)

var :: String -> Parser Int
var prefix = do
  char '$'
  string prefix
  xs <- many digit
  return (read xs)

inVar :: Parser Snip
inVar = do
  x <- var "in"
  return (InVar x)

outVar :: Parser Snip
outVar = do
  x <- var "out"
  return (OutVar x)

text :: Parser Snip
text = do
  xs <- many1 (noneOf ['$'])
  return (Snip xs)

snip :: Parser Snip
snip = do
  try inVar <|> try outVar <|> text <?> "snip"
  
parseSnip :: String -> [Snip]
parseSnip t = 
  case parse (many snip) "snip" t of
    Left err -> error (show err) -- TODO
    Right snips -> snips
