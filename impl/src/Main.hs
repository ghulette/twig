import Prelude hiding (catch)
import Control.Exception
import Data.List (intercalate)
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

trace :: Trace CBlock
trace ins outs txt = mkCBlock (size ins) (size outs) txt

runOne :: (Env Proc) -> (Id,Term) -> IO ()
runOne defs (x,t) = do
  putStrLn $ "Applying rule " ++ x ++ " to term " ++ (show t)
  putStr $ show t
  case run x defs trace t of
    Just (b,t') -> do
      putStr " -> "
      print t'
      let (txt,inputs,outputs) = render "_twig" b
      putStrLn $ "Inputs: " ++ (intercalate ", " inputs)
      putStrLn $ "Outputs: " ++ (intercalate ", " outputs)
      putStrLn $ "Code:"
      putStrLn txt
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
