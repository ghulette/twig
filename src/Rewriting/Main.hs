import Prelude hiding (catch)
import Control.Exception
import System.Environment (getArgs)
import Rewriting.Parser
import Rewriting.RuleExpr
import Rewriting.Term

-- Front end

parse :: String -> IO Rules
parse x = case parseRules x of
  Left err -> fail (show err)
  Right env -> return env

parseInput :: String -> IO [Term]
parseInput x = case parseTerms x of
  Left err -> fail (show err)
  Right terms -> return terms

runOne :: Rules -> Term -> IO ()
runOne env t = do
  putStr (show t)
  case run "main" env t of
    Just (t',_) -> do
      putStr " -> "
      print t'
    Nothing -> putStrLn " No match"
  `catch` \(RuntimeException msg) -> do 
    putStrLn "Error"
    putStrLn msg

main :: IO ()
main = do
  [rulesFile] <- getArgs
  rulesInput <- readFile rulesFile
  env <- parse rulesInput
  termsInput <- getContents
  terms <- parseInput termsInput
  mapM_ (runOne env) terms
