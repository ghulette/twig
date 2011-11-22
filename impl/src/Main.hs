import Prelude hiding (catch)
import Control.Exception
import System.Environment (getArgs)
import Twig.AST
import Twig.Parser
import Twig.RuleExpr
import Twig.Term
import Twig.Env (Env)
import Twig.Block.Lang.C


-- Front end

parse :: String -> IO (Env Proc)
parse x = case parseAST x of
  Left err -> fail (show err)
  Right ast -> return (compile ast)

parseInput :: String -> IO [(Id,Term)]
parseInput x = case parseTerms x of
  Left err -> fail (show err)
  Right terms -> return terms

-- outputTrace :: Trace -> IO ()
-- outputTrace m = do
--   let ns = [1..] :: [Int]
--   let ss = evalSupply ["gen" ++ (show x) | x <- ns] m
--   mapM_ putStrLn ss

runOne :: (Env Proc) -> (Id,Term) -> IO ()
runOne env (x,t) = do
  putStrLn $ "Applying rule " ++ x ++ " to term " ++ (show t)
  putStr $ show t
  let runf = run :: Id -> Env Proc -> Strategy CBlock
  case runf x env t of
    Just (_,t') -> do
      putStr " -> "
      print t'
      --outputTrace m
    Nothing -> putStrLn " -> *** No match ***"
  `catch` \(RuntimeException msg) -> do 
    putStrLn $ " -> Error: " ++ msg

main :: IO ()
main = do
  [rulesFile] <- getArgs
  rulesInput <- readFile rulesFile
  env <- parse rulesInput
  termsInput <- getContents
  terms <- parseInput termsInput
  mapM_ (\x -> runOne env x >> putStrLn "") terms
