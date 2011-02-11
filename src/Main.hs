import CodeGen
import Strategy
import Typemap.Jni
import Term

runExample :: Show a => Strategy a CodeGenProc -> Ident -> a -> IO ()
runExample rule x t = do
  putStrLn $ "Input=  " ++ x ++ ":" ++ (show t)
  case rule t of
    Just (t',gen) -> do
      let (x',code) = runCodeGenProc genSyms x gen
      putStrLn $ "Output= " ++ x' ++ ":" ++ (show t')
      putStrLn $ render code
      putStrLn $ "-----"
    Nothing -> do
      putStrLn $ "No mapping"

main :: IO ()
main = do
  runExample convertTo "cstr" (CPtr CChar)
  runExample convertFrom "jstr" JavaString
  runExample (convertFrom `seqn` convertTo) "jstr" JavaString
  runExample (success `seqn` convertTo) "jstr" JavaString
  runExample convertFrom "foo" (CFunc CVoid [("x",CPtr CChar),
                                             ("y",CPtr CChar)])
