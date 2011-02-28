{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CodeGen
  ( Ident
  , CodeGen
  , CodeGenProc (..)
  , module Code
  , runCodeGenProc
  , bind
  , local
  , writeStmt
  , writeBlock
  , var
  ) where

import Code
import Data.Monoid
import qualified GenSym
import GenSym (GenSym,Sym,evalGenSym)
import Control.Monad

type Ident = Sym

newtype CodeGen a = CodeGen (GenSym Code a)
  deriving (Functor,Monad)

newtype CodeGenProc = CodeGenProc (Ident -> CodeGen Ident)

instance Monoid CodeGenProc where
  mempty = CodeGenProc $ return
  (CodeGenProc m1) `mappend` (CodeGenProc m2) = CodeGenProc $ m1 >=> m2

run :: [Ident] -> CodeGen a -> (a,Code)
run vars (CodeGen m) = evalGenSym vars m

runCodeGenProc :: [Ident] -> Ident -> CodeGenProc -> (Ident,Code)
runCodeGenProc vars x (CodeGenProc m) = run vars (m x)

bind :: Char -> Ident -> CodeGen ()
bind k v = CodeGen $ GenSym.bindVar k v

var :: Char -> CodeGen String
var x = CodeGen $ GenSym.var x

local :: CodeGen a -> CodeGen a
local (CodeGen m) = CodeGen $ (GenSym.doLocal m)

writeStmt :: String -> CodeGen ()
writeStmt s = CodeGen $ do
  s' <- GenSym.replaceVars s
  GenSym.write (stmt s')

writeBlock :: String -> String -> CodeGen ()
writeBlock s1 s2 = CodeGen $ do
  s1' <- GenSym.replaceVars s1
  s2' <- GenSym.replaceVars s2
  GenSym.write (block s1' s2')
