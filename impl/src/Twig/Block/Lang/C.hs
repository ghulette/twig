{-# LANGUAGE DeriveDataTypeable #-}

module Twig.Block.Lang.C (CBlock,render,mkCBlock,parseCType) where

import Control.Exception
import Data.Typeable (Typeable)
import Twig.Block
import Twig.Block.Lang.C.Parser
import Control.Monad.Supply

-- Exceptions

data CBlockException = ParseException String
                     | InvalidEltException String
                     deriving (Typeable,Show)

instance Exception CBlockException

-- Types

type Id = String

data CType = Void
           | Char
           | Short
           | Int
           | Long
           | Float
           | Double
           | Ptr CType
           deriving (Eq,Show)

data CBlockElt = InVar Int
               | OutVar Int
               | Text String
               deriving (Show)

data CBlock = Basic Int Int [CBlockElt]
            | Permute Int [Int]
            | Seq CBlock CBlock
            | Par CBlock CBlock
            deriving (Show)

-- Block interface

instance Block CBlock where  
  permute n outs = Permute n outs
  invalid = undefined

  inputs (Basic n _ _) = n
  inputs (Par b1 b2) = inputs b1 + inputs b2
  inputs (Seq b1 _) = inputs b1
  inputs (Permute n _) = n 

  outputs (Basic _ n _) = n
  outputs (Par b1 b2) = outputs b1 + outputs b2
  outputs (Seq _ b2) = outputs b2
  outputs (Permute _ outs) = length outs

  par b1 b2 = Par b1 b2
  seqn b1 b2 | outputs b1 == inputs b2 = Seq b1 b2
             | otherwise = invalid

-- C-specific functions

parseCType :: String -> Maybe CType
parseCType c = case c of
  "int" -> Just Int
  "char" -> Just Char
  "float" -> Just Float
  "ptr(char)" -> Just (Ptr Char)
  _ -> Nothing

varIds :: Id -> [Id]
varIds prefix = map ((prefix ++) . show) [(1 :: Integer)..]

render :: String -> CBlock -> (String,[String],[String])
render prefix b = (flip evalSupply) (varIds prefix) $ do
  inVars <- supplies (inputs b)
  (txt,outVars) <- renderM b inVars
  return (txt,inVars,outVars)

renderM :: CBlock -> [Id] -> Supply Id (String,[Id])
renderM (Basic _ outn ts) inVars = do
  outVars <- supplies outn
  let txt = concatMap (renderElt inVars outVars) ts
  return (txt,outVars)
renderM (Permute numIns mapping) inVars = renderM permuteBlk inVars
  where numOuts = length mapping
        f = \o i -> [OutVar o,Text "=",InVar i,Text ";\n"]
        body = concat (zipWith f [1..numOuts] mapping)
        permuteBlk = Basic numIns numOuts body
renderM (Seq b1 b2) inVars = do
  (txt1,midVars) <- renderM b1 inVars
  (txt2,outVars) <- renderM b2 midVars
  return (txt1++txt2,outVars)
renderM (Par b1 b2) inVars = do
  let (inVars1,inVars2) = splitAt (inputs b1) inVars
  (txt1,outVars1) <- renderM b1 inVars1
  (txt2,outVars2) <- renderM b2 inVars2
  return (txt1++txt2,outVars1++outVars2)

renderElt :: [Id] -> [Id] -> CBlockElt -> String
renderElt _ _ (Text s) = s
renderElt invars _ (InVar x) = invars !! (x-1)
renderElt _ outvars (OutVar x) = outvars !! (x-1)

convertElt :: VarTextElt -> CBlockElt
convertElt t = case t of 
  Lit s -> Text s
  Var "in" n -> InVar n
  Var "out" n -> OutVar n
  Var x _ -> throw (InvalidEltException (x ++ " is not a valid variable name"))

mkCBlock :: Int -> Int -> String -> CBlock
mkCBlock numIn numOut s = 
  case parseTextWithVars s of
    Left err -> throw (ParseException (show err))
    Right ts -> Basic numIn numOut (map convertElt ts)
