import Supply
import CodeGen

main :: IO ()
main = do
  let Just c1 = code [Float,Float] [Float] 
                [OutVar 0,Snip " = ",InVar 0,Snip " + ",InVar 1,Snip ";"]
  let Just c2 = code [Float] [Float] 
                [OutVar 0,Snip " = ",InVar 0,Snip " + 1;"]
  let Just c = par c1 c2
  let gen = map (\x -> "gen" ++ (show x)) [1..]
  let (t,ins,outs) = evalSupply gen (render c)
  putStrLn t
  print ins
  print outs
