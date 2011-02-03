import Code

type Id = String

data Type = CVoid
          | CChar
          | CPtr Type
          | CFunc Type [(Id,Type)]
          | JavaString
          | JavaMethod Type [(Id,Type)]
          deriving (Eq,Show)

type Term = (Id,Type)

type Rewrite a = a -> Maybe a

type Rule = Rewrite Term

-- To should be its own inverse?  But side-effects should be different for
-- inverted function?
convertFrom :: Rule
convertFrom (x,CPtr CChar) = do
  let pre = ["const jbyte *str;",
             "str = (*env)->GetStringUTFChars(env, prompt, NULL);",
             "if (str == NULL) {return NULL;}",
             "char *cstr = (char *)str;"]
  let post = "(*env)->ReleaseStringUTFChars(env, prompt, str);"
  let code = block (unlines pre) post
  return (x,JavaString)
convertFrom (x,JavaString) = do
  return (x,CPtr CChar)
convertFrom _ = 
  Nothing

freeVars :: [Id]
freeVars = [replicate k ['a'..'z'] | k <- [1..]] >>= sequence

-- code :: CodeGen ()
-- code = do
--   x <- genSym
--   tell $ stmt ("foo " ++ x)

exTerm1 :: Term
exTerm1 = ("foo",CFunc CVoid [("x",CPtr CChar),("y",CPtr CChar)])

main :: IO ()
main = do
  print exTerm1
  let m = convertFrom exTerm1
  print m
