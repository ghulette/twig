module Twig.Reduction (normalize) where

import Twig.RuleExpr

reduce :: Eq a => (a -> a) -> a -> a
reduce f x = case f x of x' | x' == x -> x'
                         x' -> reduce f x'

normalize :: RuleExpr -> RuleExpr
normalize (Seq a (LeftChoice b c)) = LeftChoice (Seq a b) (Seq a c)
normalize (Seq (LeftChoice a b) c) = LeftChoice (Seq a c) (Seq b c)
normalize (Test e) = Test (normalize e)
normalize (Neg e) = Neg (normalize e)
normalize (Fix x e) = Fix x (normalize e)
normalize (Path i e) = Path i (normalize e)
normalize (BranchOne e) = BranchOne (normalize e)
normalize (BranchAll e) = BranchAll (normalize e)
normalize (BranchSome e) = BranchSome (normalize e)
normalize (Congruence es) = Congruence (map normalize es)
normalize x = x
