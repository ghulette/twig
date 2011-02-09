module Strategy where

import Control.Arrow
import CodeGen

type Strategy a = a -> CodeGen a

success :: Strategy a
success = return

failure :: Strategy a
failure _ = abort

-- These need "try" or something like that
-- test :: Strategy a -> Strategy a
-- test s x = undefined
--   
-- neg :: Strategy a -> Strategy a
-- neg s x = undefined

seqn :: Strategy a -> Strategy a -> Strategy a
seqn s1 s2 = runKleisli $ (Kleisli s1) >>> (Kleisli s2)

-- MonadPlus is not well defined
-- choice :: Strategy a -> Strategy a -> Strategy a
-- choice s1 s2 = runKleisli $ (Kleisli s1) <+> (Kleisli s2)
