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

userTypes :: String -> Maybe CType
userTypes c = case c of
  "polarf" -> Just (Struct "PolarF")
  "polard" -> Just (Struct "PolarD")
  "pydouble" -> Just (Ptr (Struct "PyObject"))
  "pytuple(pydouble,pydouble)" -> Just (Ptr (Struct "PyObject"))
  "address" -> Just (Ptr (Struct "Address"))
  "py(string)" -> Just (Ptr (Struct "PyObject"))
  "py(int)" -> Just (Ptr (Struct "PyObject"))
  "py(json(int))" -> Just (Ptr (Struct "PyObject"))
  "py(json(string))" -> Just (Ptr (Struct "PyObject"))
  "py(json(string,int))" -> Just (Ptr (Struct "PyObject"))
  "json(string,int)" -> Just (Ptr (Struct "json_t"))
  "json(string)" -> Just (Ptr (Struct "json_t"))
  "json(int)" -> Just (Ptr (Struct "json_t"))
  "string" -> Just (Ptr Char)
  _        -> Nothing

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
  let termToCType = mapM (parseCType userTypes . show) . flatten
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
      let (txt,inputs,outputs) = render "gen" b
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
  termsInput <- getContents
  terms <- parseInput termsInput
  mapM_ (runOne env mainRule) terms
