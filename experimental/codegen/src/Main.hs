import Supply
import CodeGen

main :: IO ()
main = do
  let c1 = code [Float,Float] [Float] (parseSnip "$out0 = $in0 + $in1;")
  let c2 = code [Float]       [Float] (parseSnip "$out0 = $in0 + 1;")
  let Just c = seqn c1 c2
  let gen = map (\x -> "gen" ++ (show x)) [(1 :: Int)..]
  let (t,ins,outs) = evalSupply gen (render c)
  putStrLn $ (show ins) ++ " -> " ++ (show outs)
  putStrLn t

