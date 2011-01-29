import Iso.MaybeWriter
import Iso.Code
import Iso.Rewrite

type Id = String

data Type = CVoid
          | CChar
          | CPtr Type
          | CFunc Type [(Id,Type)]
          | JavaString
          | JavaMethod Type [(Id,Type)]
          deriving (Eq,Show)

type Term = (Id,Type)

type Rule = Rewrite Code Term

-- To should be its own inverse?  But side-effects should be different for
-- inverted function?
convertFrom :: Rule
convertFrom (x,CPtr CChar) = do
  let pre = ["const jbyte *str;",
             "str = (*env)->GetStringUTFChars(env, prompt, NULL);",
             "if (str == NULL) {return NULL;}",
             "char *cstr = (char *)str;"]
  let post = "(*env)->ReleaseStringUTFChars(env, prompt, str);"
  write (block (unlines pre) post)
  return (x,JavaString)
convertFrom (x,JavaString) = do
  return (x,CPtr CChar)
convertFrom _ = do
  failure  

example :: Term
example = ("foo",CFunc CVoid [("x",CPtr CChar),("y",CPtr CChar)])

main :: IO ()
main = do
  print example
  let m = convertFrom example
  print m
