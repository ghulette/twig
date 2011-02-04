import Code
import CodeGen

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

-- To should be its own inverse?  But side-effects should be different for
-- inverted function?
convertFrom :: Rule
convertFrom (x,JavaString) = do
  y <- genSym
  z <- genSym
  let pre = ["const jbyte *" ++ y ++ ";",
             y ++ " = (*env)->GetStringUTFChars(env, " ++ x ++ ", NULL);",
             "if(" ++ y ++ " == NULL) {return NULL;}",
             "char *" ++ z ++ " = (char *)" ++ y ++ ";"]
  let post = "(*env)->ReleaseStringUTFChars(env, " ++ x ++ ", " ++ y ++ ");"
  let code = block (unlines pre) post
  writeCode code
  return $ Just (z,CPtr CChar)
convertFrom (x,CPtr CChar) = do
  y <- genSym
  let pre = ["jstring " ++ y ++ ";",
             y ++ " = (*env)->NewStringUTF(env, " ++ x ++");"]
  let code = stmt (unlines pre)
  writeCode code
  return $ Just (x,JavaString)
convertFrom _ = 
  return Nothing

freeVars :: [Id]
freeVars = ["_gen" ++ (show i) | i <- [(0 :: Integer)..]]

-- ex1 :: Term
-- ex1 = ("foo",CFunc CVoid [("x",CPtr CChar),("y",CPtr CChar)])

main :: IO ()
main = do
  let ex2 = ("my_string",CPtr CChar)
  print ex2
  let (t,c,_) = evalCodeGen (convertFrom ex2) freeVars
  print t
  putStrLn (render c)
