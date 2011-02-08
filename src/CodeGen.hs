{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CodeGen 
  ( Ident
  , CodeGen
  , evalCodeGen
  , genSym
  , writeCode
  , replaceSyms
  , clearEnv
  , bind
  , getVar
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

bind :: Char -> Ident -> CodeGen ()
bind x v = CodeGen $ lift $ lift $ store x v

clearEnv :: CodeGen ()
clearEnv = CodeGen $ lift $ lift $ reset

genSym :: CodeGen Ident
genSym = CodeGen $ lift $ supply

writeCode :: Code -> CodeGen ()
writeCode c = CodeGen $ tell c

getVar :: Char -> CodeGen String
getVar x = do
  mbvar <- fetch x
  case mbvar of 
    Just y -> return y
    Nothing -> do
      y <- genSym
      bind x y
      return y

replaceSyms :: String -> CodeGen String
replaceSyms "" = return ""
replaceSyms ('$' : '{' : x : '}' : xs) = do
  sym <- getVar x
  xs' <- replaceSyms xs
  return (sym ++ xs')
replaceSyms (x:xs) = do
  xs' <- replaceSyms xs
  return (x:xs')
