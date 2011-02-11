import Code
import CodeGen
import Strategy
import Data.List (intercalate)

data Type = CVoid
          | CChar
          | CPtr Type
          | CFunc Type [(Ident,Type)]
          | JavaString
          | JavaMethod Type [(Ident,Type)]
          deriving (Eq,Show)

jn :: [String] -> String
jn = intercalate "\n"

gen1 :: CodeGenProc
gen1 = CodeGenProc $ \x -> do
  let a = jn ["const jbyte* ${y};",
              "${y} = (*env)->GetStringUTFChars(env, ${x}, NULL);",
              "if(${y} == NULL) {return NULL;}",
              "char* ${z} = (char *)${y};"]
  let b = "(*env)->ReleaseStringUTFChars(env, ${x}, ${y});"
  clearVars
  bindVar 'x' x
  writeBlock a b
  z <- var 'z'
  return z

gen2 :: CodeGenProc
gen2 = CodeGenProc $ \x -> do
  let a = jn ["jstring ${y};",
              "${y} = (*env)->NewStringUTF(env, ${x});"]
  clearVars
  bindVar 'x' x
  writeStmt a
  y <- var 'y'
  return y

type Rule = Strategy Type CodeGenProc

convert :: Rule
convert JavaString = Just (CPtr CChar,gen1)
convert (CPtr CChar) = Just (JavaString,gen2)
convert _ = Nothing

genCode :: CodeGenProc -> Ident -> (Ident,Code)
genCode gen x = runCodeGenProc freeVars x gen
  where freeVars = ["_gen" ++ (show i) | i <- [(0 :: Integer)..]]

runExample :: Rule -> Type -> Ident -> IO ()
runExample rule t x = do
  putStrLn $ "Input=  " ++ x ++ ":" ++ (show t)
  case rule t of
    Just (t',gen) -> do
      let (x',code) = genCode gen x
      putStrLn $ "Output= " ++ x' ++ ":" ++ (show t')
      putStrLn $ "Code"
      putStrLn $ render code
    Nothing -> do
      putStrLn $ "Failed"

main :: IO ()
main = do
  runExample convert (CPtr CChar) "cstr"
  runExample convert JavaString "jstr"
  runExample (convert `seqn` convert) JavaString "jstr"
  runExample (success `seqn` convert) JavaString "jstr"
  runExample convert (CFunc CVoid [("x",CPtr CChar),("y",CPtr CChar)]) "foo"
