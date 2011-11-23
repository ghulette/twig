module Twig.Lexer where

import Text.Parsec
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (haskellStyle)

type Lexer s a = Parsec [Char] s a

-- Tokens

lexer :: Tok.TokenParser s
lexer = Tok.makeTokenParser haskellStyle 
  { Tok.reservedOpNames = [";","|","+","?","~","->","=","#","<<<",">>>"]
  , Tok.reservedNames = ["inv","rule","def","T","F","fix","one","some","all"]
  }

lexeme :: Lexer s u -> Lexer s u
lexeme = Tok.lexeme lexer

natural :: Lexer s Integer
natural = Tok.natural lexer

parens :: Lexer s u -> Lexer s u
parens = Tok.parens lexer

comma :: Lexer s ()
comma = Tok.comma lexer >> return ()

brackets :: Lexer s u -> Lexer s u
brackets = Tok.brackets lexer

braces :: Lexer s u -> Lexer s u
braces = Tok.braces lexer

angles :: Lexer s u -> Lexer s u
angles = Tok.angles lexer

reserved :: String -> Lexer s ()
reserved = Tok.reserved lexer

reservedOp :: String -> Lexer s ()
reservedOp = Tok.reservedOp lexer

identifier :: Lexer s String
identifier = Tok.identifier lexer

stringLiteral :: Lexer s String
stringLiteral = Tok.stringLiteral lexer

allOf :: Lexer s u -> Lexer s u
allOf p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r
