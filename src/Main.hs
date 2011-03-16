import Prelude hiding (catch)
import Control.Exception
import System.Environment (getArgs)
import Parser
import RuleExpr
import Term
import Env (Env)
import Supply

-- Front end

parse :: String -> IO (Env Proc)
parse x = case parseRules x of
  Left err -> fail (show err)
  Right env -> return env

parseInput :: String -> IO [Term]
parseInput x = case parseTerms x of
  Left err -> fail (show err)
  Right terms -> return terms

outputTrace :: Trace -> IO ()
outputTrace m = do
  let ns = [1..] :: [Int]
  let ss = evalSupply ["gen" ++ (show x) | x <- ns] m
  mapM_ putStrLn ss

runOne :: (Env Proc) -> Term -> IO ()
runOne env t = do
  putStr (show t)
  case run "main" env (t,"x") of
    Just ((t',l),m) -> do
      putStr " -> "
      print t'
      outputTrace m
    Nothing -> putStrLn " -> No match"
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
