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

data CBlock = Basic [CType] [CType] [CBlockElt]
            | Permute Int [Int]
            | Seq CBlock CBlock
            | Par CBlock CBlock
            deriving (Show)

type CDecl = (Id,CType)

-- Block interface

instance Block CBlock where  
  permute n outs = Permute n outs
  invalid = undefined

  inputs (Basic inTypes _ _) = length inTypes
  inputs (Par b1 b2) = inputs b1 + inputs b2
  inputs (Seq b1 _) = inputs b1
  inputs (Permute n _) = n 

  outputs (Basic _ outTypes _) = length outTypes
  outputs (Par b1 b2) = outputs b1 + outputs b2
  outputs (Seq _ b2) = outputs b2
  outputs (Permute _ outs) = length outs

  par b1 b2 = Par b1 b2
  seqn b1 b2 | outputs b1 == inputs b2 = Seq b1 b2
             | otherwise = invalid

-- C-specific functions

-- This should be expanded into an actual parser?
parseCType :: String -> Maybe CType
parseCType c = case c of
  "void"      -> Just Void
  "char"      -> Just Char
  "short"     -> Just Short
  "int"       -> Just Int
  "long"      -> Just Long
  "float"     -> Just Float
  "double"    -> Just Double
  "ptr(char)" -> Just (Ptr Char)
  _           -> Nothing
  
renderCType :: CType -> String
renderCType c = case c of
  Void   -> "void"
  Char   -> "char"
  Short  -> "short"
  Int    -> "int"
  Long   -> "long"
  Float  -> "float"
  Double -> "double"
  Ptr x  -> "*" ++ renderCType x

varIds :: Id -> [Id]
varIds prefix = map ((prefix ++) . show) [(1 :: Integer)..]


-- Rendering CBlocks to a string, input vars, and output vars.

render :: String -> CBlock -> (String,[String],[String])
render prefix b = (flip evalSupply) (varIds prefix) $ do
  inVars <- supplies (inputs b)
  (decls,txt,outVars) <- renderM b inVars
  let header = unlines (map renderDecl decls)
  return (header ++ txt,inVars,outVars)

renderM :: CBlock -> [Id] -> Supply Id ([CDecl],String,[Id])
renderM (Basic _ outTypes ts) inVars = do
  outVars <- supplies (length outTypes)
  let decls = zip outVars outTypes
  let txt = concatMap (renderElt inVars outVars) ts
  return (decls,txt,outVars)
renderM (Permute _ mapping) inVars = do
  let outVars = map (\i -> inVars !! (pred i)) mapping
  return ([],"",outVars)
renderM (Seq b1 b2) inVars = do
  (decl1,txt1,midVars) <- renderM b1 inVars
  (decl2,txt2,outVars) <- renderM b2 midVars
  return (decl1++decl2,txt1++txt2,outVars)
renderM (Par b1 b2) inVars = do
  let (inVars1,inVars2) = splitAt (inputs b1) inVars
  (decl1,txt1,outVars1) <- renderM b1 inVars1
  (decl2,txt2,outVars2) <- renderM b2 inVars2
  return (decl1++decl2,txt1++txt2,outVars1++outVars2)

renderElt :: [Id] -> [Id] -> CBlockElt -> String
renderElt _ _ (Text s) = s
renderElt invars _ (InVar x) = invars !! (x-1)
renderElt _ outvars (OutVar x) = outvars !! (x-1)

renderDecl :: CDecl -> String
renderDecl (x,t) = (renderCType t) ++ " " ++ x

-- Parsing CBlocks from a string

mkCBlock :: [CType] -> [CType] -> String -> Maybe CBlock
mkCBlock ins outs s = 
  case parseTextWithVars s of
    Left _ -> Nothing
    Right ts -> Just $ Basic ins outs (map convertElt ts)

convertElt :: VarTextElt -> CBlockElt
convertElt t = case t of 
  Lit s -> Text s
  Var "in" n -> InVar n
  Var "out" n -> OutVar n
  Var x _ -> throw (InvalidEltException (x ++ " is not a valid variable name"))

