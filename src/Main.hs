import CodeGen
import Strategy
import Typemap.Jni

runExample :: Show a => Strategy a CodeGenProc -> a -> Ident -> IO ()
runExample rule t x = do
  putStrLn $ "Input=  " ++ x ++ ":" ++ (show t)
  case rule t of
    Just (t',gen) -> do
      let (x',code) = runCodeGenProc genSyms x gen
      putStrLn $ "Output= " ++ x' ++ ":" ++ (show t')
      putStrLn $ "Code"
      putStrLn $ render code
    Nothing -> do
      putStrLn $ "No mapping"

main :: IO ()
main = do
  runExample convert (CPtr CChar) "cstr"
  runExample convert JavaString "jstr"
  runExample (convert `seqn` convert) JavaString "jstr"
  runExample (success `seqn` convert) JavaString "jstr"
  runExample convert (CFunc CVoid [("x",CPtr CChar),("y",CPtr CChar)]) "foo"
