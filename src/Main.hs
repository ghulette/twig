import Code
import CodeGen
import Data.List (intercalate)


type Id = String

data Type = CVoid
          | CChar
          | CPtr Type
          | CFunc Type [(Id,Type)]
          | JavaString
          | JavaMethod Type [(Id,Type)]
          deriving (Eq,Show)

type Term = (Id,Type) 

type Rule = Term -> CodeGen (Maybe Term)

jn :: [String] -> String
jn = intercalate "\n"

-- To should be its own inverse?  But side-effects should be different for
-- inverted function?
convertFrom :: Rule
convertFrom (x,JavaString) = do
  let a = jn ["const jbyte* ${y};",
              "${y} = (*env)->GetStringUTFChars(env, ${x}, NULL);",
              "if(${y} == NULL) {return NULL;}",
              "char* ${z} = (char *)${y};"]
  let b = "(*env)->ReleaseStringUTFChars(env, ${x}, ${y});"
  clearVars
  bindVar 'x' x
  writeBlock a b
  z <- var 'z'
  return $ Just (z,CPtr CChar)
convertFrom (x,CPtr CChar) = do
  let a = jn ["jstring ${y};",
              "${y} = (*env)->NewStringUTF(env, ${x});"]
  clearVars
  bindVar 'x' x
  writeStmt a
  y <- var 'y'
  return $ Just (y,JavaString)
convertFrom _ = 
  return Nothing

freeVars :: [Id]
freeVars = ["_gen" ++ (show i) | i <- [(0 :: Integer)..]]

-- ex1 :: Term
-- ex1 = ("foo",CFunc CVoid [("x",CPtr CChar),("y",CPtr CChar)])

runExample :: Term -> IO ()
runExample t = do
  putStrLn $ "Input: " ++ show t
  let (t',code) = evalCodeGen (convertFrom t) freeVars
  putStrLn $ "Output: " ++ show t'
  putStrLn $ "Code:"
  putStrLn $ render code

main :: IO ()
main = do
  runExample ("cstr",CPtr CChar)
  runExample ("jstr",JavaString)
  runExample ("foo",CFunc CVoid [("x",CPtr CChar),("y",CPtr CChar)])
