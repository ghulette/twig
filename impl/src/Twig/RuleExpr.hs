{-# LANGUAGE DeriveDataTypeable #-}

module Twig.RuleExpr where

import Control.Exception
import Data.Typeable (Typeable)
import Data.List (foldl')
import Control.Monad (when)
import Control.Monad.Supply
import Data.Monoid
import Twig.Pattern (Pattern,match,build)
import Twig.Env (Env)
import qualified Twig.Env as Env
import Twig.Term
import Twig.Util.StringSub
import Twig.Util.MonadWriter ()
import Twig.Block.Lang.C
import Twig.Block


-- Runtime exceptions

data EvalException = RuntimeException String deriving (Typeable,Show)

instance Exception EvalException

runtimeErr :: String -> a
runtimeErr msg = throw (RuntimeException msg)


-- Expression types

type Id = String
data Proc = Proc [Id] RuleExpr
type Strategy a = Term -> Maybe (a,Term)

-- Environment

bindVars :: Env (Strategy a) -> [(Id,Strategy a)] -> Env (Strategy a)
bindVars = foldl' $ flip $ uncurry Env.bind

makeRuleEnv :: [(Id,Proc)] -> Env Proc
makeRuleEnv = Env.fromList

-- Expressions

data RuleExpr = Call Id [RuleExpr]
              | Rule Pattern Pattern String
              | Success
              | Failure
              | Test RuleExpr
              | Neg RuleExpr
              | Fix Id RuleExpr
              | VarExpr Id
              | Seq RuleExpr RuleExpr
              | LeftChoice RuleExpr RuleExpr
              | Choice RuleExpr RuleExpr
              | Path Integer RuleExpr
              | BranchOne RuleExpr
              | BranchAll RuleExpr
              | BranchSome RuleExpr
              | Congruence [RuleExpr]

eval :: Block a => RuleExpr -> Env Proc -> Env (Strategy a) -> Strategy a
eval (Rule lhs rhs m) _ _ t = do
  bindings <- match lhs t
  t' <- build bindings rhs
  let m' = invalid -- <-- Fix this!
  -- let m' = fmap (fmap (replace (Env.toList bindings))) m
  return (m',t')
eval Success _ _ t = Just (identity (size t),t)
eval Failure _ _ _ = Nothing
eval (Test e) defs env t = 
  case eval e defs env t of
    Just _ -> Just (identity (size t),t)
    Nothing -> Nothing
eval (Neg e) defs env t = 
  case eval e defs env t of
    Just _ -> Nothing
    Nothing -> Just (identity (size t),t)
eval (Seq e1 e2) defs env t =
  case eval e1 defs env t of
    Nothing -> Nothing
    Just (m1,t') -> 
      case eval e2 defs env t' of
        Just (m2,t'') -> Just (m1 `seqn` m2,t'')
        Nothing -> Nothing
eval (LeftChoice e1 e2) defs env t =
  case eval e1 defs env t of
    Just (t',m) -> Just (t',m)
    Nothing -> 
      case eval e2 defs env t of
        Just (t',m) -> Just (t',m)
        Nothing -> Nothing
eval (Choice _ _) _ _ _ = undefined
-- eval (Choice e1 e2) defs env t =
-- -- Probably we don't want non-det choice for Twig, but it is interesting
-- -- as a general rewriting expression.
-- case (eval e1 defs env t,eval e2 defs env t) of
--   (Just (x,m1),Just (x',_)) -> 
--     if x == x' 
--       then Just (x,m1) -- Note: arbitrarily choose first trace!
--       else runtimeErr "Non-confluence"
--   (Just (t',m),Nothing) -> Just (t',m)
--   (Nothing,Just (t',m)) -> Just (t',m)
--   (Nothing,Nothing) -> Nothing
-- eval (Path 0 e) defs env t = 
--   -- #0(s) just applies s to root
--   eval e defs env t 
-- eval (Path i e) defs env t = 
--   pathM (fromInteger i) (eval e defs env) t
-- eval (BranchOne e) defs env t =
--   oneM (eval e defs env) t
-- eval (BranchAll e) defs env t =
--   allM (eval e defs env) t
-- eval (BranchSome e) defs env t =
--   someM (eval e defs env) t
-- eval (Congruence es) defs env t = do
--   let fs = map (\e -> eval e defs env) es
--   congruenceM fs t
-- eval (Fix x e) defs env t = s t
--   where s = eval e defs env'
--         env' = Env.bind x s env
eval (VarExpr x) defs env t =
  case Env.lookup x env of
    Nothing -> eval (Call x []) defs env t
    Just s -> s t
eval (Call x args) defs env t =
  case Env.lookup x defs of
    Nothing -> runtimeErr $ "Variable " ++ x ++ " not in scope"
    Just (Proc params e) -> do
      when (length args < length params) $ runtimeErr "Not enough args"
      when (length args > length params) $ runtimeErr "Too many args"
      let ss = map (\arg -> eval arg defs env) args
      let env' = bindVars env (zip params ss)
      eval e defs env' t

reduce :: Eq a => (a -> a) -> a -> a
reduce f x = case f x of x' | x' == x -> x'
                         x' -> reduce f x'

normalize :: RuleExpr -> RuleExpr
normalize (Seq a (LeftChoice b c)) = LeftChoice (Seq a b) (Seq a c)
normalize (Seq (LeftChoice a b) c) = LeftChoice (Seq a c) (Seq b c)
normalize (Test e) = Test (normalize e)
normalize (Neg e) = Neg (normalize e)
normalize (Fix x e) = Fix x (normalize e)
normalize (Path i e) = Path i (normalize e)
normalize (BranchOne e) = BranchOne (normalize e)
normalize (BranchAll e) = BranchAll (normalize e)
normalize (BranchSome e) = BranchSome (normalize e)
normalize (Congruence es) = Congruence (map normalize es)
normalize x = x

run :: Block a => Id -> Env Proc -> Strategy a
run entry defs t = 
  case Env.lookup entry defs of 
    Just (Proc [] e) -> do
      (t',ms) <- eval e defs Env.empty t
      return (t',ms)
    Just (Proc _ _) -> runtimeErr (entry ++ " cannot have args")
    Nothing -> runtimeErr (entry ++ " is not defined")
