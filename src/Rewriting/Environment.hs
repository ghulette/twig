module Rewriting.Environment where

-- Environment

type Env a = [(String,a)]

empty :: Env a
empty = []

extend :: String -> a -> Env a -> Env a
extend x t e = (x,t) : e

fetch :: String -> Env a -> Maybe a
fetch = lookup
