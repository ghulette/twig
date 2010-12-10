import Data.List (find)
import System.Environment (getArgs)
import Rewriting.Combinators (path)
import Rewriting.Error
import Rewriting.Parser
import Rewriting.Rule
import Rewriting.Term
import Rewriting.Util


type Rules = [RuleDef]

lookupRuleDef :: Rules -> String -> Maybe RuleDef
lookupRuleDef xs x = find (\(RuleDef x' _ _) -> x == x') xs

eval :: Rules -> RuleExpr -> Term -> Either Error (Maybe Term)
eval _ (RuleLit s) t = return (apply s t)
eval _ Success t = return (Just t)
eval _ Failure _ = return Nothing
eval _ (Path i) t = return (path (fromInteger i) t)
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
          let s' = sub (zip params args) s
          eval env s' t

sub :: [(String,RuleExpr)] -> RuleExpr -> RuleExpr
sub env (RuleVar x) = 
  case lookup x env of 
    Just s -> s
    Nothing -> RuleVar x
sub env (Test s) = Test (sub env s)
sub env (Neg s) = Neg (sub env s)
sub env (Seq s1 s2) = Seq (sub env s1) (sub env s2)
sub env (Choice s1 s2) = Choice (sub env s1) (sub env s2)
sub env (BranchAll s) = BranchAll (sub env s)
sub env (BranchOne s) = BranchOne (sub env s)
sub env (BranchSome s) = BranchSome (sub env s)
sub env (Congruence ss) = Congruence (map (sub env) ss)
sub env (Call x ss) = Call x (map (sub env) ss)
sub _ t@(RuleLit _) = t
sub _ t@Success = t
sub _ t@Failure = t
sub _ t@(Path _) = t

run :: Rules -> Term -> Either Error (Maybe Term)
run env t = case lookupRuleDef env mainRuleId of 
  Just (RuleDef _ [] s) -> eval env s t
  Just (RuleDef _ _ _ ) -> evalError "main should not have args"
  Nothing -> evalError "main is undefined"
  where
    mainRuleId = "main"


-- Front end

parse :: String -> IO Rules
parse x = case parseRules x of
  Left err -> fail (show err)
  Right env -> do
    mapM_ print env
    return env

parseInput :: String -> IO [Term]
parseInput x = case parseTerms x of
  Left err -> fail (show err)
  Right terms -> return terms

runOne :: Rules -> Term -> IO ()
runOne env t = do
  putStr (show t)
  putStr " -> "
  case run env t of
    Left err -> fail (show err)
    Right x -> case x of
      Just t' -> print t'
      Nothing -> putStrLn "No match"

main :: IO ()
main = do
  [rulesFile] <- getArgs
  rulesInput <- readFile rulesFile
  env <- parse rulesInput
  termsInput <- getContents
  terms <- parseInput termsInput
  mapM_ (runOne env) terms
