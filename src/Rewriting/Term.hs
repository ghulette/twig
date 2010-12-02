module Rewriting.Term where

import Data.List (intercalate)

data Term = Var String
          | Const String [Term]
          deriving Eq

data Rule = Rule Term Term deriving Eq

instance Show Term where
  show (Var x) = "\'" ++ x
  show (Const k ts) = k ++ "(" ++ (intercalate "," tss) ++ ")"
    where tss = map show ts

instance Show Rule where
  show (Rule t1 t2) = (show t1) ++ " -> " ++ (show t2)
