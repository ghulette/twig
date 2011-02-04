module CodeGen where

import Control.Monad.Identity
import Control.Monad.Supply
import Control.Monad.Writer
import Code

type Ident = String

type CodeGen a = WriterT Code (SupplyT Ident Identity) a

genSym :: CodeGen Ident
genSym = lift supply

writeCode :: Code -> CodeGen ()
writeCode = tell

evalCodeGen :: CodeGen a -> [Ident] -> (a,Code,[Ident])
evalCodeGen m vars = (x,cs,vars')
  where m1 = runWriterT m
        m2 = runSupplyT m1 vars
        ((x,cs),vars') = runIdentity m2
