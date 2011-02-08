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
  (a',e1) <- replaceSyms (bind [('x',x)]) a
  (b',e2) <- replaceSyms e1 b
  writeCode (block a' b')
  let Just z = fetch e2 'z'
  return $ Just (z,CPtr CChar)
convertFrom (x,CPtr CChar) = do
  let a = jn ["jstring ${y};",
              "${y} = (*env)->NewStringUTF(env, ${x});"]
  (a',_) <- replaceSyms (bind [('x',x)]) a
  writeCode (stmt a')
  return $ Just (x,JavaString)
convertFrom _ = 
  return Nothing

freeVars :: [Id]
freeVars = ["_gen" ++ (show i) | i <- [(0 :: Integer)..]]

-- ex1 :: Term
-- ex1 = ("foo",CFunc CVoid [("x",CPtr CChar),("y",CPtr CChar)])

runExample :: Term -> IO ()
runExample t = do
  putStrLn $ "Input: " ++ show t
  let (t',code,_) = evalCodeGen (convertFrom t) freeVars
  putStrLn $ "Output: " ++ show t'
  putStrLn $ "Code:"
  putStrLn $ render code

main :: IO ()
main = do
  runExample ("my_string",CPtr CChar)
  runExample ("foo",JavaString)
