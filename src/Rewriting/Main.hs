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
eval (BranchAll t) env = branchAll (eval t env)
eval (BranchOne t) env = branchOne (eval t env)
eval (BranchSome t) env = branchSome (eval t env)
eval (Path i) _ = path (fromInteger i)

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
