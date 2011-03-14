module CodeGen.Code where

import Data.Monoid

type Stmt = String

data Code = Block [Stmt] [Stmt] deriving (Eq,Show)

stmt :: Stmt -> Code
stmt s = Block [s] []

block :: Stmt -> Stmt -> Code
block s1 s2 = Block [s1] [s2]

noop :: Code
noop = Block [] []

nest :: Code -> Code -> Code
(Block s11 s12) `nest` (Block s21 s22) = Block (s11 ++ s21) (s22 ++ s12)

render :: Code -> String
render (Block s1 s2) = (unlines s1) ++ (unlines s2)

instance Monoid Code where
  mempty = noop
  mappend = nest
