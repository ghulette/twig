import Data.List
import Control.Monad
import System.Random

-- Term roots and the number of children
spec = [("and",2)
       ,("or",2)
       ,("not",1)
       ,("true",0)
       ,("false",0)
       ,("p",0)
       ,("q",0)
       ,("r",0)]

-- Weights for each term in the random selection
weights = [0.2,0.2,0.2,0.1,0.1,0.1,0.1,0.1]

choose :: Ord a => a -> [(a,b)] -> b
choose _ [] = undefined
choose _ ((_,t):[]) = t
choose i ((x,t):xs) = if i <= x then t else choose i xs

weightedRandom :: (Num a,Ord a,Random a) => [(a,b)] -> IO b
weightedRandom pairs = do
  let (weights,xs) = unzip pairs
  i <- randomRIO (0,sum weights)
  let x = choose i (zip (scanl1 (+) weights) xs)
  return x

randomTerm :: [Double] -> [(String,Int)] -> IO String
randomTerm weights spec = do
  (t,n) <- weightedRandom (zip weights spec)
  if n == 0 
    then return t 
    else do
      ts <- replicateM n (randomTerm weights spec)
      return (t ++ "(" ++ (intercalate "," ts) ++ ")")

main :: IO ()
main = do
  t <- randomTerm weights spec
  putStrLn t
