{-# LANGUAGE DeriveDataTypeable #-}

module Rewriting.RuleExpr where

import Data.List (find)
import Rewriting.Rule
import Rewriting.Term
import Rewriting.Util

import Data.Typeable
import Control.Exception
import Control.Monad (guard,when)

data EvalException = RuntimeException String deriving (Show,Typeable)

instance Exception EvalException

runtimeErr :: String -> a
runtimeErr msg = throw (RuntimeException msg)

-- Expressions

type Id = String

data RuleExpr = RuleVar Id
              | RuleLit Rule
              | Call Id [RuleExpr]
              | Success
              | Failure
              | Test RuleExpr
              | Neg RuleExpr
              | Seq RuleExpr RuleExpr
              | LeftChoice RuleExpr RuleExpr
              | Choice RuleExpr RuleExpr
              | Path Integer RuleExpr
              | BranchAll RuleExpr
              | BranchOne RuleExpr
              | BranchSome RuleExpr
              | Congruence [RuleExpr]
              deriving (Eq,Show)

traverse :: (RuleExpr -> Maybe RuleExpr) -> RuleExpr -> RuleExpr
traverse f (Test s) = update f (Test (traverse f s))
traverse f (Neg s) = update f (Neg (traverse f s))
traverse f (Seq s1 s2) = update f (Seq (traverse f s1) (traverse f s2))
traverse f (LeftChoice s1 s2) = 
  update f (LeftChoice (traverse f s1) (traverse f s2))
traverse f (Choice s1 s2) = update f (Choice (traverse f s1) (traverse f s2))
traverse f (BranchAll s) = update f (BranchAll (traverse f s))
traverse f (BranchOne s) = update f (BranchOne (traverse f s))
traverse f (BranchSome s) = update f (BranchSome (traverse f s))
traverse f (Congruence ss) = update f (Congruence (map (traverse f) ss))
traverse f (Call x ss) = update f (Call x (map (traverse f) ss))
traverse f s = update f s

data RuleDef = RuleDef Id [Id] RuleExpr deriving (Eq,Show)

type Rules = [RuleDef]

lookupRuleDef :: Rules -> String -> Maybe RuleDef
lookupRuleDef xs x = find (\(RuleDef x' _ _) -> x == x') xs

eval :: Rules -> RuleExpr -> Term -> Maybe Term
eval _ (RuleLit s) t = apply s t
eval _ Success t = Just t
eval _ Failure _ = Nothing
eval env (Test s) t =
  case eval env s t of
    Just _ -> Just t
    Nothing -> Nothing
eval env (Neg s) t =
  case eval env s t of
    Just _ -> Nothing
    Nothing -> Just t
eval env (Seq s1 s2) t = 
  case eval env s1 t of
    Just t' -> eval env s2 t'
    Nothing -> Nothing
eval env (LeftChoice s1 s2) t =
  case eval env s1 t of
    Just t' -> Just t'
    Nothing -> eval env s2 t
eval env (Choice s1 s2) t =
  case (eval env s1 t,eval env s2 t) of
    (Just x,Just x') -> 
      if x == x' 
        then Just x 
        else runtimeErr "Non-confluence"
    (Just t',Nothing) -> Just t'
    (Nothing,Just t') -> Just t'
    (Nothing,Nothing) -> Nothing
eval env (Path 0 s) t = 
  eval env s t -- #0(s) just applies s to root
eval env (Path i s) t = do
  let ts = children t
  ts' <- path (fromInteger i) (eval env s) ts
  return (t `withChildren` ts')
eval env (BranchAll s) t = do
  let ts = children t
  ts' <- mapAll (eval env s) ts
  return (t `withChildren` ts')
eval env (BranchOne s) t = do
  let ts = children t
  ts' <- mapOne (eval env s) ts
  return (t `withChildren` ts')
eval env (BranchSome s) t = do
  let ts = children t
  ts' <- mapSome (eval env s) ts
  return (t `withChildren` ts')
eval env (Congruence ss) t = do
  let ts = children t
  let rs = map (eval env) ss
  guard (length ts == length rs)
  ts' <- sequence (zipWith ($) rs ts)
  return (t `withChildren` ts')
eval env (RuleVar x) t = 
  case lookupRuleDef env x of
    Nothing -> runtimeErr ("Not in scope: " ++ x)
    Just (RuleDef _ [] s) -> eval env s t
    Just _ -> runtimeErr ("Missing args in call to " ++ x)
eval env (Call x args) t =
  case lookupRuleDef env x of 
    Nothing -> runtimeErr ("Not in scope: " ++ x)
    Just (RuleDef _ params s) -> do
      when (length args < length params) (runtimeErr "Not enough args")
      when (length args > length params) (runtimeErr "Too many args")
      let s' = subVars (zip params args) s
      eval env s' t

subVars :: [(String,RuleExpr)] -> RuleExpr -> RuleExpr
subVars env = traverse sub
  where sub (RuleVar x) = lookup x env
        sub _ = Nothing

-- We can do this with Generics instead, but it is really slow:
-- sub :: [(String,RuleExpr)] -> RuleExpr -> RuleExpr
-- sub env = everywhere (mkT sub')
--   where sub' (RuleVar x) = (lookup x env) `withDefault` (RuleVar x) 
--         sub' t = t

run :: String -> Rules -> Term -> Maybe Term
run entry env t = 
  case lookupRuleDef env entry of 
    Just (RuleDef _ [] s) -> eval env s t
    Just (RuleDef _ _ _) -> runtimeErr (entry ++ " cannot have args")
    Nothing -> runtimeErr (entry ++ " is not defined")
