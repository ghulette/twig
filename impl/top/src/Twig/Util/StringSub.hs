module Twig.Util.StringSub (replace) where

import Text.Regex
import Data.List (foldl')

replace :: Show a => [(String,a)] -> String -> String
replace = flip $ foldl' repl
  where repl = \inp (pat,rep) -> subRegex (mkRegex pat) inp (show rep)
