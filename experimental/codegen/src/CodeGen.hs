module CodeGen where

import Supply

type Id = String

data Snip = Snip String | In Int | Out Int
data Code = Code { numInputs :: Int, numOutputs :: Int, snippet :: [Snip] }
          | Seqn Code Code
          | Congr [Code]
          | Branch [Code]

seqn :: Code -> Code -> Supply Id Code
seqn c1 c2 | numOutputs c1 /= numInputs c2 = error "seqn"
seqn c1 c2 = do
  ids <- supplies (numOutputs c1)
  let c1snip' = map (rewriteOutput ids) (snippet c1)
  let c2snip' = map (rewriteInput ids) (snippet c2)
  let snip' = c1snip' ++ c2snip'
  return (Code (numInputs c1) (numOutputs c2) snip')
  
branch :: [Code] -> Code
branch = undefined

congruence :: [Code] -> Code
congruence = undefined

wrap :: (Code,Code) -> Code -> Code
wrap = undefined

rewriteInput :: [Id] -> Snip -> Snip
rewriteInput ids (In i) = Snip (ids !! i)
rewriteInput _ x = x

rewriteOutput :: [Id] -> Snip -> Snip
rewriteOutput ids (Out i) = Snip (ids !! i)
rewriteOutput _ x = x
