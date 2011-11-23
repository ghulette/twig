import Prelude hiding (catch)
import Control.Exception
import Data.Maybe (fromJust)
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

mkBlock :: BlockBuilder CBlock
mkBlock inn outn txt = fromJust (mkCBlock inn outn txt)

runOne :: (Env Proc) -> (Id,Term) -> IO ()
runOne defs (x,t) = do
  putStrLn $ "Applying rule " ++ x ++ " to term " ++ (show t)
  putStr $ show t
  case run x defs mkBlock t of
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
