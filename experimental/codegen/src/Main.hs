import Supply
import CodeGen
import Snip

main :: IO ()
main = do
  let Just c = do c1 <- code [Float,Float] [Float] (parseSnip "$out0 = $in0 + $in1;")
                  c2 <- code [Float]       [Float] (parseSnip "$out0 = $in0 + 1;")
                  seqn c1 c2
  let gen = map (\x -> "gen" ++ (show x)) [1..]
  let (t,ins,outs) = evalSupply gen (render c)
  putStrLn t
  print ins
  print outs
