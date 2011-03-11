{-# LANGUAGE DeriveDataTypeable #-}

module RuleExpr where

import Control.Exception
import Data.Typeable (Typeable)
import Data.List (foldl')
import Control.Monad (guard,when)
import Data.Monoid
import Rule (Rule,apply)
import Env (Env,Id)
import qualified Env
import Term
import Util
import StringSub


-- Runtime exceptions

data EvalException = RuntimeException String deriving (Typeable,Show)

instance Exception EvalException

runtimeErr :: String -> a
runtimeErr msg = throw (RuntimeException msg)


-- Values

type AbstractStrategy a b = a -> Maybe (a,b)

type Trace = String
data Proc = Proc [Id] RuleExpr deriving (Eq,Show)
type Strategy = AbstractStrategy Term [Trace]


-- Environment

bindVars :: Env Strategy -> [(Id,Strategy)] -> Env Strategy
bindVars = foldl' $ flip $ uncurry Env.bind

makeRuleEnv :: [(Id,Proc)] -> Env Proc
makeRuleEnv = Env.fromList

-- Expressions

data RuleExpr = RuleCall Id [RuleExpr]
              | RuleLit Rule [Trace]
              | Success
              | Failure
              | Test RuleExpr
              | Neg RuleExpr
              | Fix Id RuleExpr
              | Var Id
              | Seq RuleExpr RuleExpr
              | LeftChoice RuleExpr RuleExpr
              | Choice RuleExpr RuleExpr
              | Path Integer RuleExpr
              | BranchOne RuleExpr
              | BranchAll RuleExpr
              | BranchSome RuleExpr
              | Congruence [RuleExpr]
              deriving (Eq,Show)

eval :: RuleExpr -> Env Proc -> Env Strategy -> Strategy
eval (RuleLit rule ms) _ _ t = do
  (t',binds) <- apply rule t
  ms' <- mapM (stringSub binds) ms
  return (t',ms')
eval Success _ _ t = Just (t,mempty)
eval Failure _ _ _ = Nothing
eval (Test e) defs env t = 
  case eval e defs env t of
    Just _ -> Just (t,mempty)
    Nothing -> Nothing
eval (Neg e) defs env t = 
  case eval e defs env t of
    Just _ -> Nothing
    Nothing -> Just (t,mempty)
eval (Seq e1 e2) defs env t =
  case eval e1 defs env t of
    Nothing -> Nothing
    Just (t',m1) -> 
      case eval e2 defs env t' of
        Just (t'',m2) -> Just (t'',m1 `mappend` m2)
        Nothing -> Nothing
eval (LeftChoice e1 e2) defs env t =
  case eval e1 defs env t of
    Just (t',m) -> Just (t',m)
    Nothing -> 
      case eval e2 defs env t of
        Just (t',m) -> Just (t',m)
        Nothing -> Nothing
eval (Choice e1 e2) defs env t =
  -- Probably we don't want non-det choice for Twig, but it is interesting
  -- as a general rewriting expression.
  case (eval e1 defs env t,eval e2 defs env t) of
    (Just (x,m1),Just (x',_)) -> 
      if x == x' 
        then Just (x,m1) -- Note: arbitrarily choose first trace!
        else runtimeErr "Non-confluence"
    (Just (t',m),Nothing) -> Just (t',m)
    (Nothing,Just (t',m)) -> Just (t',m)
    (Nothing,Nothing) -> Nothing
eval (Path 0 e) defs env t = eval e defs env t -- #0(s) just applies s to root
eval (Path i e) defs env t = do
  let ts = children t
  let s = eval e defs env
  (ts',m) <- path (fromInteger i) s ts
  return (t `withChildren` ts',m)
eval (BranchOne e) defs env t = do
  let ts = children t
  (ts',m) <- mapOne (eval e defs env) ts
  return (t `withChildren` ts',m)
eval (BranchAll e) defs env t = do
  let ts = children t
  (ts',m) <- mapAll (eval e defs env) ts
  return (t `withChildren` ts',m)
eval (BranchSome e) defs env t = do
  let ts = children t
  (ts',m) <- mapSome (eval e defs env) ts
  return (t `withChildren` ts',m)
eval (Congruence es) defs env t = do
  let ts = children t
  let rs = map (\e -> eval e defs env) es
  guard $ length ts == length rs
  mts' <- sequence (zipWith ($) rs ts)
  let (ts',ms) = unzip mts'
  return (t `withChildren` ts',mconcat ms)
eval (Fix x e) defs env t = s t
  where s = eval e defs env'
        env' = Env.bind x s env
eval (Var x) defs env t =
  case Env.lookup x env of
    Nothing -> eval (RuleCall x []) defs env t
    Just s -> s t
eval (RuleCall x args) defs env t =
  case Env.lookup x defs of
    Nothing -> runtimeErr $ "Variable " ++ x ++ " not in scope"
    Just (Proc params e) -> do
      when (length args < length params) $ runtimeErr "Not enough args"
      when (length args > length params) $ runtimeErr "Too many args"
      let ss = map (\arg -> eval arg defs env) args
      let env' = bindVars env (zip params ss)
      eval e defs env' t

run :: Id -> Env Proc -> Strategy
run entry defs t = 
  case Env.lookup entry defs of 
    Just (Proc [] e) -> do
      (t',ms) <- eval e defs Env.empty t
      return (t',ms)
    Just (Proc _ _) -> runtimeErr (entry ++ " cannot have args")
    Nothing -> runtimeErr (entry ++ " is not defined")
