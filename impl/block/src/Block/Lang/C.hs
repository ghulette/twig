module Block.Lang.C where

import Block
import Block.Supply

type Id = String

data CBlockElt = InVar Int
               | OutVar Int
               | Text String

data CBlock = Basic Int Int [CBlockElt]
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

render :: CBlock -> [Id] -> Supply Id (String,[Id])
render (Basic _ outn ts) invars = do
  outvars <- supplies outn
  let txt = concatMap (renderElt invars outvars) ts
  return (txt,outvars)
render (Permute numIns ins) invars = render basicBlock invars
  where numOuts = length ins
        outs = [1..numOuts]
        eltf = \(o,i) -> [OutVar o,Text "=",InVar i,Text ";\n"]
        elts = concatMap eltf (zip outs ins)
        basicBlock = Basic numIns numOuts elts
-- render (Seq b1 b2) invars = undefined
-- render (Par b1 b2) invars = undefined

renderElt :: [Id] -> [Id] -> CBlockElt -> String
renderElt _ _ (Text s) = s
renderElt invars _ (InVar x) = invars !! x
renderElt _ outvars (OutVar x) = outvars !! x
