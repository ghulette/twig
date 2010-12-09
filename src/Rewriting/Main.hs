import System.Environment (getArgs)
import Control.Monad
import Rewriting.Parser
import Rewriting.Term

data Error = RuntimeError String deriving Eq

instance Show Error where
  show (RuntimeError msg) = "Runtime error: " ++ msg

evalError :: String -> Either Error a
evalError msg = Left (RuntimeError msg)

instance Monad (Either a) where
  return = Right
  (Left x) >>= _ = Left x
  (Right x) >>= f = f x

type Env = [RuleStmt]

fetchRule :: Env -> String -> Maybe RuleExpr
fetchRule [] _ = Nothing
fetchRule ((RuleDef y t):_) x | x == y = Just t
fetchRule (_:rs) x = fetchRule rs x

-- fetchMacro :: Env -> String -> Maybe ([String],RuleExpr)
-- fetchMacro [] _ = Nothing
-- fetchMacro ((MacroDef y params t):_) x | x == y = Just (params,t)
-- fetchMacro (_:rs) x = fetchMacro rs x
-- 
-- extend :: Env -> String -> RuleExpr -> Env
-- extend rs x e = (RuleDef x e) : rs

mapAll :: (a -> Either b (Maybe a)) -> [a] -> Either b (Maybe [a])
mapAll _ [] = Right (Just [])
mapAll f (x:xs) = 
  case f x of
    Left err -> Left err
    Right Nothing -> Right Nothing
    Right (Just x') -> 
      case mapAll f xs of
        Left err -> Left err
        Right Nothing -> Right Nothing
        Right (Just xs') -> Right (Just (x':xs'))

mapOne :: (a -> Either b (Maybe a)) -> [a] -> Either b (Maybe [a])
mapOne _ [] = Right Nothing
mapOne f (x:xs) = 
  case f x of
    Left err -> Left err
    Right (Just x') -> Right (Just (x':xs))
    Right Nothing -> 
      case mapOne f xs of
        Left err -> Left err
        Right Nothing -> Right Nothing
        Right (Just xs') -> Right (Just (x:xs'))

mapSome :: (a -> Either b (Maybe a)) -> [a] -> Either b (Maybe [a])
mapSome _ [] = Right Nothing
mapSome f (x:xs) = 
  case f x of
    Left err -> Left err
    Right (Just x') ->
      case mapSome f xs of
        Left err -> Left err
        Right Nothing -> Right (Just (x':xs))
        Right (Just xs') -> Right (Just (x':xs'))
    Right Nothing -> 
      case mapSome f xs of
        Left err -> Left err
        Right Nothing -> Right Nothing
        Right (Just xs') -> Right (Just (x:xs'))

eval :: Env -> RuleExpr -> Term -> Either Error (Maybe Term)
eval _ (RuleLit s) t = return (apply s t)
eval _ Success t = return (success t)
eval _ Failure t = return (failure t)
eval _ (Path i) t = return (path (fromInteger i) t)
eval _ (Root x) t = return (root x t)
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
  mts' <- mapAll (eval env s) ts
  case mts' of
    Just ts' -> return (Just (t `withChildren` ts'))
    Nothing -> return Nothing
eval env (BranchOne s) t = do
  let ts = children t
  mts' <- mapOne (eval env s) ts
  case mts' of
    Just ts' -> return (Just (t `withChildren` ts'))
    Nothing -> return Nothing
eval env (BranchSome s) t = do
  let ts = children t
  mts' <- mapSome (eval env s) ts
  case mts' of
    Just ts' -> return (Just (t `withChildren` ts'))
    Nothing -> return Nothing
-- eval (Congruence ts) env = do
--   ts' <- mapM (\t -> eval t env) ts
--   return (congruence ts')
-- eval (RuleVar x) env = 
--   case fetchRule env x of 
--     Just t -> do
--       f <- eval t env
--       return f
--     Nothing -> 
--       evalError ("fetch " ++ x)
-- eval (Macro _ _) _ = 
--   evalError "Macros are not supported"

run :: Env -> Term -> Either Error (Maybe Term)
run env t = case fetchRule env "main" of 
  Just s -> eval env s t
  Nothing -> evalError "main is undefined"


-- Front end

parse :: String -> IO Env
parse x = case parseRules x of
  Left err -> fail (show err)
  Right env -> do
    mapM_ print env
    return env

parseInput :: String -> IO [Term]
parseInput x = case parseTerms x of
  Left err -> fail (show err)
  Right terms -> return terms

runOne :: Env -> Term -> IO ()
runOne env t = do
  print t
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
