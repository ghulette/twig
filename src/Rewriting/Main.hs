import Rewriting.Parser
import Rewriting.Term

eval :: RuleExpr -> [(String,RuleExpr)] -> Term -> Maybe Term
eval (RuleVar x) env = case lookup x env of Just t -> eval t env
                                            Nothing -> undefined
eval (RuleLit r) _ = apply r
eval Success _ = success
eval Failure _ = failure
eval (Test t) env = test (eval t env)
eval (Neg t) env = neg (eval t env)
eval (Seq t1 t2) env = seqn (eval t1 env) (eval t2 env)
eval (Choice t1 t2) env = choice (eval t1 env) (eval t2 env)

main :: IO ()
main = do
  input <- getContents
  case parseRules input of
    Left err -> print err
    Right defs -> do
      mapM_ print defs
      let s = eval (RuleVar "lt") defs
      let Right t1 = parseTerm "lt(s(s(zero)), s(zero))"
      let Right t2 = parseTerm "lt(s(s(zero)), s(s(s(zero))))"
      let Right t3 = parseTerm "lt(s(s(zero)), s(s(zero))))"
      print t1
      print $ s t1
      print t2
      print $ s t2
      print t3
      print $ s t3
      

-- main :: IO ()
-- main = do
--   let Right ([(x,r)]) = parseRules "r1 = s(X) -> X"
--   let Right t = parseTerm "n(n( s(s(zero)) , s(zero) ), s(s(zero)) )"
--   print x
--   print r
--   print t
--   let Just t1 = applyTopDown r t
--   let Just t2 = applyTopDown r t1
--   let Just t3 = applyTopDown r t2
--   print t1
--   print t2
--   print t3
