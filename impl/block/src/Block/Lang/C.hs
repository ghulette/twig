module Block.Lang.C (render,mkCBlock) where

import Block
import Block.Lang.Parser
import Control.Monad.Supply

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

varIds :: Id -> [Id]
varIds prefix = map ((prefix ++) . show) [(1 :: Integer)..]

render :: String -> CBlock -> (String,[String],[String])
render prefix b = (flip evalSupply) (varIds prefix) $ do
  inVars <- supplies (inputs b)
  (txt,outVars) <- renderM b inVars
  return (txt,inVars,outVars)

renderM :: CBlock -> [Id] -> Supply Id (String,[Id])
renderM (Basic _ outn ts) inVars = do
  outVars <- supplies outn
  let txt = concatMap (renderElt inVars outVars) ts
  return (txt,outVars)
renderM (Permute numIns ins) inVars = renderM permuteBlk inVars
  where numOuts = length ins
        f = \o i -> [OutVar o,Text "=",InVar i,Text ";\n"]
        body = concat (zipWith f [1..numOuts] ins)
        permuteBlk = Basic numIns numOuts body
renderM (Seq b1 b2) inVars = do
  (txt1,midVars) <- renderM b1 inVars
  (txt2,outVars) <- renderM b2 midVars
  return (txt1++txt2,outVars)
renderM (Par b1 b2) inVars = do
  let (inVars1,inVars2) = splitAt (inputs b1) inVars
  (txt1,outVars1) <- renderM b1 inVars1
  (txt2,outVars2) <- renderM b2 inVars2
  return (txt1++txt2,outVars1++outVars2)

renderElt :: [Id] -> [Id] -> CBlockElt -> String
renderElt _ _ (Text s) = s
renderElt invars _ (InVar x) = invars !! x
renderElt _ outvars (OutVar x) = outvars !! x

convertElt :: VarTextElt -> Maybe CBlockElt
convertElt (Lit s) = Just (Text s)
convertElt (Var "in" n) = Just (InVar n)
convertElt (Var "out" n) = Just (OutVar n)
convertElt _ = Nothing

mkCBlock :: Int -> Int -> String -> Maybe CBlock
mkCBlock inn outn s = 
  case parseTextWithVars s of
    Left _ -> Nothing
    Right varText -> do
      elts <- mapM convertElt varText
      return (Basic inn outn elts)
