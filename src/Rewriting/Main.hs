import Rewriting.Parser
import Rewriting.Reduce

main :: IO ()
main = do
  input <- getContents
  case parseRules input of
    Left err -> print err
    Right rs -> do
      mapM_ print rs
      let Right t = parseTerm "lt(s(zero),s(s(zero)))"
      let r = rs !! 2
      putStrLn "---"
      print t
      print r
      print $ rewrite r t
