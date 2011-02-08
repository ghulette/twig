{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CodeGen 
  ( Ident
  , CodeGen
  , genSym
  , writeCode
  , evalCodeGen
  , Bindings
  , bind
  , fetch
  , replaceSyms
  , replaceSymsFresh
  ) where

import Control.Monad.Identity
import Control.Monad.Supply
import Control.Monad.Writer
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Code

type Ident = String

newtype CodeGen a = CodeGen (WriterT Code (SupplyT Ident Identity) a)
  deriving (Functor,Monad)

genSym :: CodeGen Ident
genSym = CodeGen $ lift supply

writeCode :: Code -> CodeGen ()
writeCode = CodeGen . tell

evalCodeGen :: CodeGen a -> [Ident] -> (a,Code,[Ident])
evalCodeGen (CodeGen m) vars = (x,cs,vars')
  where m1 = runWriterT m
        m2 = runSupplyT m1 vars
        ((x,cs),vars') = runIdentity m2

-- This part should maybe live in its own module?

type Bindings = Map Char Ident

getVar :: Bindings -> Char -> CodeGen String
getVar env x =
  case Map.lookup x env of 
    Just y -> return y
    Nothing -> genSym

doReplaceSyms :: String -> StateT Bindings CodeGen String
doReplaceSyms "" = return ""
doReplaceSyms ('$' : '{' : x : '}' : xs) = do
  env <- get
  sym <- lift (getVar env x)
  put (Map.insert x sym env)
  xs' <- doReplaceSyms xs
  return (sym ++ xs')
doReplaceSyms (x:xs) = do
  xs' <- doReplaceSyms xs
  return (x:xs')

bind :: [(Char,Ident)] -> Bindings
bind = Map.fromList

fetch :: Bindings -> Char -> Maybe Ident
fetch env x = Map.lookup x env

replaceSyms :: Bindings -> String -> CodeGen (String,Bindings)
replaceSyms env s = runStateT (doReplaceSyms s) env

replaceSymsFresh :: String -> CodeGen (String,Bindings)
replaceSymsFresh = replaceSyms (bind [])
