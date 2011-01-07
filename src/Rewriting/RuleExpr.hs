{-# LANGUAGE DeriveDataTypeable #-}

module Rewriting.RuleExpr where

import Control.Exception
import Control.Monad (guard,when)
import Data.List (find)
import Data.Monoid
import Data.Typeable
import Rewriting.Rule
import Rewriting.Term
import Rewriting.Util


-- Runtime exceptions

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
              | Print String
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

-- We define these test and neg separately because otherwise the type of the
-- returned mempty monoid is ambiguous.
test :: Monoid m => Term -> Maybe (Term,m) -> Maybe (Term,m)
test t (Just _) = Just (t,mempty)
test _ Nothing = Nothing

neg :: Monoid m => Term -> Maybe (Term,m) -> Maybe (Term,m)
neg _ (Just _) = Nothing
neg t Nothing = Just (t,mempty)

eval :: Rules -> RuleExpr -> Term -> Maybe (Term,Trace)
eval _ (RuleLit s) t = do
  t' <- apply s t
  return (t',mempty)
eval _ Success t = Just (t,mempty)
eval _ Failure _ = Nothing
eval env (Test s) t = test t (eval env s t)
eval env (Neg s) t = neg t (eval env s t)
eval env (Seq s1 s2) t = 
  case eval env s1 t of
    Nothing -> Nothing
    Just (t',m1) -> 
      case eval env s2 t' of
        Just (t'',m2) -> Just (t'',m1 `mappend` m2)
        Nothing -> Nothing
eval env (LeftChoice s1 s2) t =
  case eval env s1 t of
    Just (t',m) -> Just (t',m)
    Nothing -> 
      case eval env s2 t of
        Just (t',m) -> Just (t',m)
        Nothing -> Nothing
eval env (Choice s1 s2) t =
  case (eval env s1 t,eval env s2 t) of
    (Just (x,m1),Just (x',_)) -> 
      if x == x' 
        then Just (x,m1) -- What do we do with the second trace here?
        else runtimeErr "Non-confluence"
    (Just (t',m),Nothing) -> Just (t',m)
    (Nothing,Just (t',m)) -> Just (t',m)
    (Nothing,Nothing) -> Nothing
eval env (Path 0 s) t = 
  eval env s t -- #0(s) just applies s to root
eval env (Path i s) t = do
  let ts = children t
  (ts',m) <- path (fromInteger i) (eval env s) ts
  return (t `withChildren` ts',m)
eval env (BranchAll s) t = do
  let ts = children t
  (ts',m) <- mapAll (eval env s) ts
  return (t `withChildren` ts',m)
eval env (BranchOne s) t = do
  let ts = children t
  (ts',m) <- mapOne (eval env s) ts
  return (t `withChildren` ts',m)
eval env (BranchSome s) t = do
  let ts = children t
  (ts',m) <- mapSome (eval env s) ts
  return (t `withChildren` ts',m)
eval env (Congruence ss) t = do
  let ts = children t
  let rs = map (eval env) ss
  guard (length ts == length rs)
  mts' <- sequence (zipWith ($) rs ts)
  let (ts',ms) = unzip mts'
  return (t `withChildren` ts',mconcat ms)
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
eval env (Print msg) t =
  return (t,[msg])

subVars :: [(String,RuleExpr)] -> RuleExpr -> RuleExpr
subVars env = traverse sub
  where sub (RuleVar x) = lookup x env
        sub _ = Nothing

-- We can do variable substitution with syb generics instead, but it is slow.
-- sub :: [(String,RuleExpr)] -> RuleExpr -> RuleExpr sub env =
-- everywhere (mkT sub')
--   where sub' (RuleVar x) = (lookup x env) `withDefault` (RuleVar x) 
--         sub' t = t

type Trace = [String]

run :: String -> Rules -> Term -> Maybe (Term,Trace)
run entry env t = 
  case lookupRuleDef env entry of 
    Just (RuleDef _ [] s) -> eval env s t
    Just (RuleDef _ _ _) -> runtimeErr (entry ++ " cannot have args")
    Nothing -> runtimeErr (entry ++ " is not defined")
