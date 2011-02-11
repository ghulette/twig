{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CodeGen
  ( Ident
  , CodeGen
  , CodeGenProc (..)
  , module Code
  , runCodeGenProc
  , genSym
  , fetchVar
  , bindVar
  , clearVars
  , writeStmt
  , writeBlock
  , replaceVars
  , var
  ) where

import Code
import Data.Monoid
import Control.Monad
import Control.Monad.WSE (WSE)
import qualified Control.Monad.WSE as WSE

type Ident = String

newtype CodeGen a = CodeGen (WSE Code Ident Char a)
  deriving (Functor,Monad)

newtype CodeGenProc = CodeGenProc (Ident -> CodeGen Ident)

instance Monoid CodeGenProc where
  mempty = CodeGenProc $ return
  (CodeGenProc m1) `mappend` (CodeGenProc m2) = CodeGenProc $ m1 >=> m2

run :: [Ident] -> CodeGen a -> (a,Code)
run vars (CodeGen m) = (x,code)
  where (x,code,_) = WSE.run vars m

runCodeGenProc :: [Ident] -> Ident -> CodeGenProc -> (Ident,Code)
runCodeGenProc vars x (CodeGenProc m) = (x',code)
  where (x',code) = run vars (m x)

fetchVar :: Char -> CodeGen (Maybe Ident)
fetchVar = CodeGen . WSE.fetch

bindVar :: Char -> Ident -> CodeGen ()
bindVar x v = CodeGen $ WSE.bind x v

clearVars :: CodeGen ()
clearVars = CodeGen $ WSE.reset

genSym :: CodeGen Ident
genSym = CodeGen $ WSE.supply

write :: Code -> CodeGen ()
write = CodeGen . WSE.tell

writeStmt :: String -> CodeGen ()
writeStmt s = do
  s' <- replaceVars s
  write (stmt s')

writeBlock :: String -> String -> CodeGen ()
writeBlock s1 s2 = do
  s1' <- replaceVars s1
  s2' <- replaceVars s2
  write (block s1' s2')

var :: Char -> CodeGen String
var x = do
  mbvar <- fetchVar x
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
