module StringSub (VarString,stringSub) where

-- TODO: This is a horribly inefficient hack. Probably could be written more
-- efficiently without Parsec.

import Text.ParserCombinators.Parsec
import Env (Env)
import qualified Env as Env
import Data.Maybe (fromJust)

type VarString = String

data Text = Const Char
          | Var String
          deriving (Eq,Show)

toString :: Show a => Env a -> Text -> Maybe String
toString _ (Const c) = return [c]
toString env (Var x) = do
  o <- Env.lookup x env
  return (show o)

varId :: Parser String
varId = do
  x <- letter
  xs <- many alphaNum
  return (x:xs)

textVar :: Parser Text
textVar = do
  _ <- char '$'
  _ <- char '{'
  x <- varId
  _ <- char '}'
  return (Var x)

textConst :: Parser Text
textConst = do
  x <- noneOf ""
  return (Const x)

text :: Parser Text
text = try textVar <|> textConst

allOf :: Parser a -> Parser a
allOf p = do
  x <- p
  eof
  return x
  
parseText :: VarString -> Either ParseError [Text]
parseText "" = Right []
parseText s = parse (allOf (many text)) "Text" s

stringSub :: Show a => Env a -> VarString -> String
stringSub env x = fromJust $
  case parseText x of
    Left _ -> error "String sub parse error - should not happen!"
    Right ts -> do
      ss <- mapM (toString env) ts
      return (concat ss)
