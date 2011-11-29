module Twig.AST where

import Twig.Env (Env)
import qualified Twig.Env as Env
import Twig.RuleExpr

data Top = Top [Unit]
data Unit = Unit [Stmt]
data Stmt = RuleStmt Id RuleExpr
          | DefStmt Id [Id] RuleExpr

compile :: Top -> Env Proc
compile (Top units) = Env.unions (map compileUnit units)

compileUnit :: Unit -> Env Proc
compileUnit (Unit stmts) = Env.fromList (map compileStmt stmts)

compileStmt :: Stmt -> (Id,Proc)
compileStmt (RuleStmt x e) = (x,Proc [] e)
compileStmt (DefStmt x ps e) = (x,Proc ps e)
