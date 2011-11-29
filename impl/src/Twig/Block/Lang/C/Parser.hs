module Twig.Block.Lang.C.Parser 
( VarTextElt (..)
, VarText
, parseTextWithVars
) where

import Text.Parsec

-- Text with embedded variables

data VarTextElt = Lit String
                | Var String Int
                deriving (Eq,Show)

type VarText = [VarTextElt]

textWithVars :: Parsec [Char] s VarText
textWithVars = do
  result <- many elt
  eof
  return result

elt :: Parsec [Char] s VarTextElt
elt = (char '$' >> var) <|> text <?> "Text or variable"

text :: Parsec [Char] s VarTextElt
text = do
  txt <- many1 (noneOf "$")
  return (Lit txt)

nat :: Parsec [Char] s Int
nat = do
  nstrh <- oneOf "123456789"
  nstrt <- many digit
  let n = read (nstrh:nstrt)
  return n

var :: Parsec [Char] s VarTextElt
var = do
  kw <- many1 letter
  n <- option 1 nat
  return (Var kw n)

parseTextWithVars :: String -> Either ParseError VarText
parseTextWithVars = parse textWithVars "(unknown)"

-- main :: IO ()
-- main = do
--   let txt = "Hello $in1 there $out2$in2"
--   case parseTextWithVars txt of
--     Left err -> print err
--     Right vt -> print vt
