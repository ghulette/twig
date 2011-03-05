import Prelude hiding (catch)
import Control.Exception
import System.Environment (getArgs)
import Parser
import RuleExpr
import Term

-- Front end

parse :: String -> IO RuleEnv
parse x = case parseRules x of
  Left err -> fail (show err)
  Right env -> do
    print env
    return env

parseInput :: String -> IO [Term]
parseInput x = case parseTerms x of
  Left err -> fail (show err)
  Right terms -> return terms

runOne :: RuleEnv -> Term -> IO ()
runOne env t = do
  putStr (show t)
  case run "main" env t of
    Just (t',ms) -> do
      putStr " -> "
      print t'
      mapM_ putStrLn ms
    Nothing -> putStrLn " No match"
  `catch` \(RuntimeException msg) -> do 
    putStrLn $ " -> Error: " ++ msg

main :: IO ()
main = do
  [rulesFile] <- getArgs
  rulesInput <- readFile rulesFile
  env <- parse rulesInput
  termsInput <- getContents
  terms <- parseInput termsInput
  mapM_ (runOne env) terms
