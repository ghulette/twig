module Env 
  ( Env
  , fromList
  , empty
  , singleton
  , bind
  , unbind
  , lookup
  , unions
  , bindUnique
  , unionsUnique
  ) where

import Prelude hiding (lookup)
import Control.Monad (guard,foldM)
import Data.Map (Map)
import qualified Data.Map as Map

type Env a = Map String a

fromList :: [(String,a)] -> Env a
fromList = Map.fromList

empty :: Env a
empty = Map.empty

singleton :: String -> a -> Env a
singleton = Map.singleton

bind :: String -> a -> Env a -> Env a
bind = Map.insert

unbind :: String -> Env a -> Env a
unbind = Map.delete

lookup :: String -> Env a -> Maybe a
lookup = Map.lookup

unions :: Eq a => [Env a] -> Env a
unions = Map.unions

-- If k is already bound, make sure we are rebinding it to the same value
bindUnique :: Eq a => String -> a -> Env a -> Maybe (Env a)
bindUnique k x m =
  case lookup k m of
    Just x' -> do
      guard (x' == x)
      return m
    Nothing -> do
      return $ Map.insert k x m

unionUnique :: Eq a => Env a -> Env a -> Maybe (Env a)
unionUnique m1 m2 =
  foldM (\m (k,v) -> bindUnique k v m) m1 (Map.assocs m2)

unionsUnique :: Eq a => [Env a] -> Maybe (Env a)
unionsUnique [] = Just empty
unionsUnique (m:ms) = foldM unionUnique m ms
