module Typemap.Jni where

import Util (br)
import CodeGen
import Strategy

genSyms :: [Ident]
genSyms = ["_gen" ++ (show i) | i <- [(0 :: Integer)..]]

data Type = CVoid
          | CChar
          | CPtr Type
          | CFunc Type [(Ident,Type)]
          | JavaString
          | JavaMethod Type [(Ident,Type)]
          deriving (Eq,Show)

gen1 :: CodeGenProc
gen1 = CodeGenProc $ \x -> local $ do
  let a = "const jbyte* ${y};" `br`
          "${y} = (*env)->GetStringUTFChars(env, ${x}, NULL);" `br`
          "if(${y} == NULL) {return NULL;}" `br`
          "char* ${z} = (char *)${y};"
  let b = "(*env)->ReleaseStringUTFChars(env, ${x}, ${y});"
  bind 'x' x
  writeBlock a b
  z <- var 'z'
  return z

gen2 :: CodeGenProc
gen2 = CodeGenProc $ \x -> local $ do
  let a = "jstring ${y};" `br`
          "${y} = (*env)->NewStringUTF(env, ${x});"
  bind 'x' x
  writeStmt a
  y <- var 'y'
  return y

convertFrom :: Strategy Type CodeGenProc
convertFrom JavaString = Just (CPtr CChar,gen1)
convertFrom (CPtr CChar) = Just (JavaString,gen2)
convertFrom _ = Nothing

-- | The convertTo function is defined in terms of convertFrom. This implies
-- that convertFrom must be bijective where it is defined (i.e. when the
-- resulting term is not Nothing).
-- This may not be well-defined with respect to code generation?
convertTo :: Strategy Type CodeGenProc
convertTo t = 
  case convertFrom t of
    Just (t',_) -> convertFrom t'
    Nothing -> Nothing
