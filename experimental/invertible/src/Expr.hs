module Expr 
  ( Expr (..)
  , eval
  ) where

import Term
import Pattern

data Expr = Rule Pattern Pattern
          | Success
          | Failure
          | Test Expr
          | Neg Expr
          | Seq Expr Expr
          | Choice Expr Expr
          | Inv Expr
          deriving (Eq,Show)

inverse :: Expr -> Maybe Expr
inverse (Inv s)     = return s
inverse (Rule l r)  = return (Rule r l)
inverse Success     = return Success
inverse Failure     = return Failure
inverse (Test s)    = return (Test s)
inverse (Neg s)     = return (Neg s)
inverse (s1 `Seq` s2) = do
  s1' <- inverse s1
  s2' <- inverse s2
  return (s2' `Seq` s1')
-- Choice is non-invertible in general because it admits non-injective 
-- functions, e.g. consider:
--   s = [x -> z] | [y -> z]
--   s y       --> z
--   (Inv s) z --> x
inverse (_ `Choice` _) = Nothing

eval :: Expr -> Term -> Maybe Term
eval (Rule l r) x = do
  env <- match l x
  x' <- build env r
  return x'
eval Success x = Just x
eval Failure _ = Nothing
eval (Test s) x = 
  case eval s x of
    Just _ -> Just x
    Nothing -> Nothing
eval (Neg s) x =
  case eval s x of
    Just _ -> Nothing
    Nothing -> Just x
eval (Seq s1 s2) x =
  case eval s1 x of
    Just x' -> eval s2 x'
    Nothing -> Nothing
eval (Choice s1 s2) x =
  case eval s1 x of
    Just x' -> Just x'
    Nothing -> eval s2 x
eval (Inv s) x = do
  s' <- inverse s
  eval s' x
