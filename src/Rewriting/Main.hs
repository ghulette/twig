import System.Environment (getArgs)
import Rewriting.Parser
import Rewriting.RuleExpr
import Rewriting.Term

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
