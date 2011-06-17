module CodeGen where

import Supply

data Snip = Snip String | Input Int | Output Int

renderSnip :: Snip -> [String] -> [String] -> String
renderSnip (Snip s) _ _ = return s
renderSnip (Input i) inputs _ = inputs !! i
renderSnip (Output i) _ outputs = outputs !! i
      
data Code = Code [Snip]
          | Seqn Code Code
          | Par Code Code

data Rendered = Rendered 
              { code :: String
              , inputs :: [String]
              , outputs :: [String]
              }

render :: Code -> [String] -> [String] -> Supply Rendered
render = undefined

