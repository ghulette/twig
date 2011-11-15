module Block.Lang.C where

import Block

data CBlock = Basic Int Int String
            | Permute Int [Int]
            | Seq CBlock CBlock
            | Par CBlock CBlock

instance Block CBlock where
  permute n outs = Permute n outs
  identity n = Permute n [1..n]
  invalid = undefined

  inputs (Basic n _ _) = n
  inputs (Par b1 b2) = inputs b1 + inputs b2
  inputs (Seq b1 _) = inputs b1
  inputs (Permute n _) = n 

  outputs (Basic _ n _) = n
  outputs (Par b1 b2) = outputs b1 + outputs b2
  outputs (Seq _ b2) = outputs b2
  outputs (Permute _ outs) = length outs

  par b1 b2 = Par b1 b2
  seqn b1 b2 | outputs b1 == inputs b2 = Seq b1 b2
             | otherwise = invalid

render :: CBlock -> String
render = undefined
