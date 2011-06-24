module CodeGen (Code,Type(..),Snip(..),code,seqn,par,render) where

import Control.Monad (guard)
import Supply
import Snip

type Id = String

data Type = Int | Float | Double | Ptr Type deriving (Eq,Show)
type InputType = Type
type OutputType = Type

data Code = Code [InputType] [OutputType] [Snip]
          | Seqn Code Code
          | Par Code Code

inputTypes :: Code -> [InputType]
inputTypes (Code ins _ _) = ins
inputTypes (Seqn c1 _) = inputTypes c1
inputTypes (Par c1 c2) = inputTypes c1 ++ inputTypes c2

outputTypes :: Code -> [OutputType]
outputTypes (Code _ outs _) = outs
outputTypes (Seqn _ c2) = outputTypes c2
outputTypes (Par c1 c2) = outputTypes c1 ++ outputTypes c2

code :: [InputType] -> [OutputType] -> [Snip] -> Maybe Code
code inputs outputs snip = do
  return (Code inputs outputs snip)

seqn :: Code -> Code -> Maybe Code
seqn c1 c2 = do
  guard $ outputTypes c1 == inputTypes c2
  return (Seqn c1 c2)

par :: Code -> Code -> Maybe Code
par c1 c2 = do
  return (Par c1 c2)

renderSnip :: [Id] -> [Id] -> Snip -> String
renderSnip _ _       (Snip s)   = s
renderSnip inputs _  (InVar i)  = inputs  !! i
renderSnip _ outputs (OutVar i) = outputs !! i

renderWithIds :: Code -> [Id] -> [Id] -> Supply Id String
renderWithIds (Code inTypes outTypes xs) inputs outputs = do
  return $ concatMap (renderSnip inputs outputs) xs
renderWithIds (Seqn c1 c2) inputs outputs = do
  xs <- supplies (length (outputTypes c1))
  t1 <- renderWithIds c1 inputs xs
  t2 <- renderWithIds c2 xs outputs
  return $ t1 ++ "\n" ++ t2
renderWithIds (Par c1 c2) inputs outputs = do
  let inputs1 = take (length (inputTypes c1)) inputs
  let inputs2 = drop (length (inputTypes c1)) inputs
  let outputs1 = take (length (outputTypes c1)) outputs
  let outputs2 = drop (length (outputTypes c1)) outputs
  t1 <- renderWithIds c1 inputs1 outputs1
  t2 <- renderWithIds c2 inputs2 outputs2
  return $ t1 ++ "\n" ++ t2

render :: Code -> Supply Id (String,[Id],[Id])
render c = do
  inputs <- supplies (length (inputTypes c))
  outputs <- supplies (length (outputTypes c))
  t <- renderWithIds c inputs outputs
  return (t,inputs,outputs)
