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

parseInput :: String -> IO [Term]
parseInput x = case parseTerms x of
  Left err -> fail (show err)
  Right terms -> return terms

trace :: Trace CBlock
trace inTerm outTerm txt = do
  let termToCType = mapM (parseCType . show) . flatten
  inCTypes <- termToCType inTerm
  outCTypes <- termToCType outTerm
  mkCBlock inCTypes outCTypes txt

runOne :: (Env Proc) -> Id -> Term -> IO ()
runOne defs rule t = do
  putStrLn $ "Applying rule " ++ rule ++ " to term " ++ (show t)
  putStr $ show t
  case run rule defs trace t of
    Just (b,t') -> do
      putStr " -> "
      print t'
      print b
      let (txt,inputs,outputs) = render "__gen" b
      putStrLn $ "Inputs: " ++ (intercalate ", " inputs)
      putStrLn $ "Outputs: " ++ (intercalate ", " outputs)
      putStrLn $ "Code:"
      putStrLn txt
    Nothing -> putStrLn " -> *** No match ***"
  `catch` \(RuntimeException msg) -> do 
    putStrLn $ " -> Error: " ++ msg

main :: IO ()
main = do
  [rulesFile,mainRule] <- getArgs
  rulesInput <- readFile rulesFile
  env <- parse rulesInput
  print env
  termsInput <- getContents
  terms <- parseInput termsInput
  mapM_ (runOne env mainRule) terms
