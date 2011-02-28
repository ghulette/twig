import CodeGen
import Strategy
import Data.Tree
import Typemap.Jni

type Strat a = Strategy (Tree a) CodeGenProc

runExample :: Show a => Strat a -> Ident -> Tree a -> IO ()
runExample rule x t = do
  putStrLn $ "Input  = " ++ x
  putStrLn $ drawTree (fmap show t)
  case rule t of
    Just (t',gen) -> do
      let (x',code) = runCodeGenProc genSyms x gen
      putStrLn $ "Output = " ++ x'
      putStrLn $ drawTree (fmap show t')
      putStrLn $ render code
      putStrLn $ "-------"
    Nothing -> do
      putStrLn $ "No mapping"

main :: IO ()
main = do
  runExample typemap "cstr" (Node CPtr [Node CChar []])
  -- runExample convertFrom "jstr" JavaString
  -- runExample (success `seqn` convertFrom) "jstr" JavaString
  runExample typemap "foo" (Node (CFunc "foo") 
                           [Node CInt [],Node Tuple
                           [Node CPtr [Node CChar []],
                            Node CPtr [Node CChar []]]])
