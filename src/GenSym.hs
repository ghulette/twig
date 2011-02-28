{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GenSym 
  ( GenSym
  , Sym
  , EnvKey
  , evalGenSym
  , genSym
  , write
  , bindVar
  , fetchVar
  , doLocal
  , var
  , replaceVars
  ) where

import Data.Monoid
import Control.Monad.State
import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as Map

type Sym = String
type EnvKey = Char
type Env = Map EnvKey Sym

data GenSymState = GenSymState
  { genSymEnv :: Env
  , genSymSupply :: [Sym]
  } deriving (Eq,Show)

mkGenSymState :: [Sym] -> GenSymState
mkGenSymState ss = GenSymState Map.empty ss

newtype GenSym w a = GenSym (WriterT w (State GenSymState) a)
  deriving (Functor,Monad)

evalGenSym :: [Sym] -> GenSym w a -> (a,w)
evalGenSym syms m = (x,w)
  where (x,w,_) = runGenSym syms m

runGenSym :: [Sym] -> GenSym w a -> (a,w,[Sym])
runGenSym syms (GenSym m) = (x,w,genSymSupply st)
  where ((x,w),st) = runState (runWriterT m) (mkGenSymState syms)

getEnv :: Monoid w => GenSym w Env
getEnv = GenSym $ do
  st <- lift get
  return (genSymEnv st)

putEnv :: Monoid w => Env -> GenSym w ()
putEnv env = GenSym $ lift (modify $ \st -> st {genSymEnv = env})

getSupply :: Monoid w => GenSym w [Sym]
getSupply = GenSym $ do
  st <- lift get
  return (genSymSupply st)

putSupply :: Monoid w => [Sym] -> GenSym w ()
putSupply syms = GenSym $ lift (modify $ \st -> st {genSymSupply = syms})
  
genSym :: Monoid w => GenSym w Sym
genSym = do
  (x:xs) <- getSupply -- will fail if supply is empty!
  putSupply xs
  return x

write :: Monoid w => w -> GenSym w ()
write = GenSym . tell

fetchVar :: Monoid w => EnvKey -> GenSym w (Maybe Sym)
fetchVar x = do
  env <- getEnv
  return (Map.lookup x env)

bindVar :: Monoid w => EnvKey -> Sym -> GenSym w ()
bindVar k v = do
  env <- getEnv
  let env' = Map.insert k v env
  putEnv env'

doLocal :: Monoid w => GenSym w a -> GenSym w a
doLocal m = do
  env <- getEnv
  x <- m
  putEnv env
  return x

var :: Monoid w => EnvKey -> GenSym w Sym
var k = do
  mbSym <- fetchVar k
  case mbSym of
    Just v -> return v
    Nothing -> do
      v <- genSym
      bindVar k v
      return v

replaceVars :: Monoid w => String -> GenSym w String
replaceVars "" = return ""
replaceVars ('$' : '{' : k : '}' : xs) = do
  v <- var k
  xs' <- replaceVars xs
  return (v ++ xs')
replaceVars (x:xs) = do
  xs' <- replaceVars xs
  return (x:xs')
