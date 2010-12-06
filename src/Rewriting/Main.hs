import System.Environment (getArgs)
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
eval (Congruence ts) env = congruence (map (\t -> eval t env) ts)
eval (Path i) _ = path (fromInteger i)
eval (Root x) _ = root x

run :: [(String,RuleExpr)] -> Term -> Maybe Term
run = run' . reverse
  where run' [] = Just
        run' rs@((x,_):_) = eval (RuleVar x) rs

main :: IO ()
main = do
  [rulesFile] <- getArgs
  rulesInput <- readFile rulesFile
  case parseRules rulesInput of
    Left err -> print err
    Right defs -> do
      mapM_ print defs
      let p = run defs
      termsInput <- getContents
      case parseTerms termsInput of
        Left err' -> print err'
        Right ts -> do
          mapM_ (print . p) ts
