{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CodeGen 
  ( Ident
  , CodeGen
  , evalCodeGen
  , genSym
  , writeStmt
  , writeBlock
  , replaceVars
  , clearVars
  , bindVar
  , var
  ) where

import Control.Monad.Identity
import Control.Monad.Supply
import Control.Monad.Writer
import Control.Monad.Env
import Code

type Ident = String

newtype CodeGen a = 
  CodeGen (WriterT Code 
          (SupplyT Ident 
          (EnvT Char Ident Identity)) a)
  deriving (Functor,Monad)

evalCodeGen :: CodeGen a -> [Ident] -> (a,Code)
evalCodeGen (CodeGen m) vars = (x,cs)
  where m1 = runWriterT m
        m2 = runSupplyT m1 vars
        m3 = evalEnvT m2
        ((x,cs),_) = runIdentity m3

fetch :: Char -> CodeGen (Maybe Ident)
fetch x = CodeGen $ lift $ lift $ load x

bindVar :: Char -> Ident -> CodeGen ()
bindVar x v = CodeGen $ lift $ lift $ store x v

genSym :: CodeGen Ident
genSym = CodeGen $ lift $ supply

clearVars :: CodeGen ()
clearVars = CodeGen $ lift $ lift $ reset

writeCode :: Code -> CodeGen ()
writeCode c = CodeGen $ tell c

writeStmt :: String -> CodeGen ()
writeStmt s = do
  s' <- replaceVars s
  writeCode (stmt s')

writeBlock :: String -> String -> CodeGen ()
writeBlock s1 s2 = do
  s1' <- replaceVars s1
  s2' <- replaceVars s2
  writeCode (block s1' s2')

var :: Char -> CodeGen String
var x = do
  mbvar <- fetch x
  case mbvar of 
    Just y -> return y
    Nothing -> do
      y <- genSym
      bindVar x y
      return y

replaceVars :: String -> CodeGen String
replaceVars "" = return ""
replaceVars ('$' : '{' : x : '}' : xs) = do
  sym <- var x
  xs' <- replaceVars xs
  return (sym ++ xs')
replaceVars (x:xs) = do
  xs' <- replaceVars xs
  return (x:xs')
