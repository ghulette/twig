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

type Term = (Ident,Type)

type Rule = Term -> CodeGen Term

jn :: [String] -> String
jn = intercalate "\n"

convert :: Rule
convert (x,JavaString) = do
  let a = jn ["const jbyte* ${y};",
              "${y} = (*env)->GetStringUTFChars(env, ${x}, NULL);",
              "if(${y} == NULL) {return NULL;}",
              "char* ${z} = (char *)${y};"]
  let b = "(*env)->ReleaseStringUTFChars(env, ${x}, ${y});"
  clearVars
  bindVar 'x' x
  writeBlock a b
  z <- var 'z'
  return (z,CPtr CChar)
convert (x,CPtr CChar) = do
  let a = jn ["jstring ${y};",
              "${y} = (*env)->NewStringUTF(env, ${x});"]
  clearVars
  bindVar 'x' x
  writeStmt a
  y <- var 'y'
  return (y,JavaString)
convert _ = abort

runExample :: Rule -> Term -> IO ()
runExample rule t = do
  putStrLn $ "Input: " ++ show t
  case evalCodeGen freeVars (rule t) of
    Just (code,t') -> do
      putStrLn $ "Output: " ++ show t'
      putStrLn $ "Code:"
      putStrLn $ render code
    Nothing -> do
      putStrLn $ "Failed"
  where freeVars = ["_gen" ++ (show i) | i <- [(0 :: Integer)..]]

main :: IO ()
main = do
  runExample convert ("cstr",CPtr CChar)
  runExample convert ("jstr",JavaString)
  runExample (convert `seqn` convert) ("jstr",JavaString)
  -- runExample (convert `seqn` success) ("jstr",JavaString)
  -- runExample (success `seqn` convert) ("jstr",JavaString)
  runExample convert ("foo",CFunc CVoid [("x",CPtr CChar),
                                         ("y",CPtr CChar)])
