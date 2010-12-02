module Rewriting.Environment where

import Rewriting.Term

type Env = [(String,Term)]

empty :: Env
empty = []

extend :: String -> Term -> Env -> Env
extend x t e = (x,t) : e

fetch :: String -> Env -> Maybe Term
fetch = lookup
