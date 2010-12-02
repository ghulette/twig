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
      putStrLn "---"
      print t
      print $ reduce rs t
