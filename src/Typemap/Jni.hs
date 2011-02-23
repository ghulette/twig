module Typemap.Jni where

import Data.Tree
import CodeGen
import Strategy
import Data.Monoid
import Control.Monad

genSyms :: [Ident]
genSyms = ["_gen" ++ (show i) | i <- [(0 :: Integer)..]]

data Type = Tuple
          | CInt
          | CChar
          | CPtr
          | CFunc Ident
          | JavaInt
          | JavaString
          | JavaMethod
          deriving (Eq,Show)

type Term = Tree Type

cDecl :: Term -> String
cDecl (Node CInt []) = "int"
cDecl (Node CChar []) = "char"
cDecl (Node CPtr [x]) = (cDecl x) ++ "*"
cDecl t = error $ "No type declaration: " ++ (show t)

cDeclGen :: Term -> Ident -> CodeGen ()
cDeclGen t x = local $ do
  bind 't' (cDecl t)
  bind 'x' x
  writeStmt "${d} ${x}"

ccallGen :: Ident -> Term -> CodeGenProc
ccallGen f t = CodeGenProc $ \x -> local $ do
  r <- var 'r'
  bind 'f' f
  bind 'x' x
  cDeclGen t r
  writeStmt "${r} = ${f}(${x});"
  return r

ccall :: Strategy Term CodeGenProc
ccall (Node Tuple [
        Node (CFunc fid) [ret,Node Tuple form],
        Node Tuple args]) = do
  guard (form == args)
  return (ret,ccallGen fid ret)
ccall _ = Nothing

-- | Converts a functions arguments and return types, performs the given
-- body transformation, and makes sure that the types match.
entry :: Strategy Term CodeGenProc -> Strategy Term CodeGenProc
entry s t = case t of
  (Node (CFunc _) [_,args]) -> do
    (args',_) <- branchAll typemap args
    (ret,_) <- s (Node Tuple [t,args'])
    return (Node JavaMethod [ret,args'],mempty)
  _ -> Nothing

typemap :: Strategy Term CodeGenProc
typemap t = case t of
  (Node JavaInt []) -> Just (Node CInt [],gen1)
  (Node CInt []) -> Just (Node JavaInt [],gen2)
  (Node JavaString []) -> Just (Node CPtr [Node CChar []],gen1)
  (Node CPtr [Node CChar []]) -> Just (Node JavaString [],gen2)
  (Node (CFunc _) [ret,args]) -> do
    (args',_) <- branchAll typemap args
    (ret',_) <- typemap ret
    return (Node JavaMethod [ret',args'],mempty)
  _ -> Nothing

gen1 :: CodeGenProc
gen1 = CodeGenProc $ \x -> local $ do
  bind 'x' x
  writeStmt "char *${z} = toCString(${x});"
  z <- var 'z'
  return z

gen2 :: CodeGenProc
gen2 = CodeGenProc $ \x -> local $ do
  bind 'x' x
  writeStmt "jstring ${z} = toJString(${x});"
  z <- var 'z'
  return z


-- gen1 :: CodeGenProc
-- gen1 = CodeGenProc $ \x -> local $ do
--   let a = "const jbyte* ${y};" `br`
--           "${y} = (*env)->GetStringUTFChars(env, ${x}, NULL);" `br`
--           "if(${y} == NULL) {return NULL;}" `br`
--           "char* ${z} = (char *)${y};"
--   let b = "(*env)->ReleaseStringUTFChars(env, ${x}, ${y});"
--   bind 'x' x
--   writeBlock a b
--   z <- var 'z'
--   return z
-- 
-- gen2 :: CodeGenProc
-- gen2 = CodeGenProc $ \x -> local $ do
--   let a = "jstring ${y};" `br`
--           "${y} = (*env)->NewStringUTF(env, ${x});"
--   bind 'x' x
--   writeStmt a
--   y <- var 'y'
--   return y
