module Block where

class Block a where
  permute :: Int -> [Int] -> a
  identity :: Int -> a
  invalid :: a
  inputs :: a -> Int
  outputs :: a -> Int
  seqn :: a -> a -> a
  par :: a -> a -> a

-- Block laws
-- Parallel composition laws
-- inputs  (x `par` y) == inputs x + inputs y
-- outputs (x `par` y) == inputs x + inputs y

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
