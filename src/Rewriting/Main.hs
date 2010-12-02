import Rewriting.Parser
import Rewriting.Reduce()

main :: IO ()
main = do
  input <- getContents
  case parseRules input of
    Left err -> print err
    Right rs -> mapM_ print rs
