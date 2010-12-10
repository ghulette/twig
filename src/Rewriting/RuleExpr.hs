module Rewriting.RuleExpr where

import Data.List (find)
import Rewriting.Error
import Rewriting.Rule
import Rewriting.Term
import Rewriting.Util

  
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

eval :: Rules -> RuleExpr -> Term -> Either Error (Maybe Term)
eval _ (RuleLit s) t = return (apply s t)
eval _ Success t = return (Just t)
eval _ Failure _ = return Nothing
eval env (Test s) t = do
  mt' <- eval env s t
  return $ case mt' of 
    Just _ -> Just t
    Nothing -> Nothing
eval env (Neg s) t = do
  mt' <- eval env s t
  return $ case mt' of
    Just _ -> Nothing
    Nothing -> Just t
eval env (Seq s1 s2) t = do
  mt' <- eval env s1 t
  case mt' of
    Just t' -> eval env s2 t'
    Nothing -> return Nothing
eval env (Choice s1 s2) t = do
  mt' <- eval env s1 t
  case mt' of 
    Just t' -> return (Just t')
    Nothing -> eval env s2 t
eval env (Path i s) t = do
  let ts = children t
  mts' <- pathM (fromInteger i) (eval env s) ts
  case mts' of 
    Just ts' -> return (Just (t `withChildren` ts'))
    Nothing -> return Nothing
eval env (BranchAll s) t = do
  let ts = children t
  mts' <- mapAllM (eval env s) ts
  case mts' of
    Just ts' -> return (Just (t `withChildren` ts'))
    Nothing -> return Nothing
eval env (BranchOne s) t = do
  let ts = children t
  mts' <- mapOneM (eval env s) ts
  case mts' of
    Just ts' -> return (Just (t `withChildren` ts'))
    Nothing -> return Nothing
eval env (BranchSome s) t = do
  let ts = children t
  mts' <- mapSomeM (eval env s) ts
  case mts' of
    Just ts' -> return (Just (t `withChildren` ts'))
    Nothing -> return Nothing
eval env (Congruence ss) t = do
  let ts = children t
  let rs = map (eval env) ss
  mts' <- zappM rs ts
  case sequence mts' of
    Just ts' -> return (Just (t `withChildren` ts'))
    Nothing -> return Nothing
eval env (RuleVar x) t = 
  case lookupRuleDef env x of 
    Nothing -> evalError ("Not in scope: " ++ x)
    Just (RuleDef _ [] s) -> eval env s t
    Just _ -> evalError ("Requires args: " ++ x)
eval env (Call x args) t =
  case lookupRuleDef env x of 
    Nothing -> evalError ("Not in scope: " ++ x)
    Just (RuleDef _ params s) -> do
      if length params /= length args
        then evalError ("Wrong number of args: " ++ x)
        else do
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

run :: Rules -> Term -> Either Error (Maybe Term)
run env t = case lookupRuleDef env mainRuleId of 
  Just (RuleDef _ [] s) -> eval env s t
  Just (RuleDef _ _ _ ) -> evalError "main should not have args"
  Nothing -> evalError "main is undefined"
  where
    mainRuleId = "main"
