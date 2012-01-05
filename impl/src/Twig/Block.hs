module Twig.Block (Block (..),BlockFunc (..)) where

class Block b where
  permute :: Int -> [Int] -> b
  identity :: Int -> b
  identity n = permute n [1..n]
  invalid :: b
  inputs :: b -> Int
  outputs :: b -> Int
  seqn :: b -> b -> b
  par :: b -> b -> b

class BlockFunc f where
  close :: Block b => f -> b -> b

-- Block laws
-- Parallel composition laws
-- inputs  (x `par` y) == inputs x + inputs y
-- outputs (x `par` y) == outputs x + outputs y

-- Sequential composition laws
-- inputs  (x `seqn` y) == inputs x
-- outputs (x `seqn` y) == outputs y

-- Identity blocks
-- identity n == permute n [1..n]
-- x `seqn` (identity (inputs x)) == x
-- (identity (inputs y)) `seqn` y == y

-- Invalid compositions
-- outputs x /= inputs y => x `seqn` y == invalid
-- invalid `seqn` y == invalid
-- x `seqn` invalid == invalid
-- invalid `par` y == invalid
-- x `par` invalid == invalid
