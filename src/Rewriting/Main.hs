import Rewriting.Parser
import Rewriting.Term

-- main :: IO ()
-- main = do
--   input <- getContents
--   case parseRules input of
--     Left err -> print err
--     Right rs -> do
--       mapM_ print rs
--       let Right t = parseTerm "ack(s(zero),s(zero))"
--       putStrLn "---"
--       print t
--       print $ reduce rs t

main :: IO ()
main = do
  let Right (r:[]) = parseRules "s(X) -> X"
  let Right t = parseTerm "n(n( s(s(zero)) , s(zero) ), s(s(zero)) )"
  print r
  print t
  let Just t1 = applyTopDown r t
  let Just t2 = applyTopDown r t1
  let Just t3 = applyTopDown r t2
  print t1
  print t2
  print t3
