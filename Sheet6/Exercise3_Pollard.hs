import System.Random

-- determines a proper divisor a of n using a random value x in {0..(n-1)}
-- randomized version
pollard :: Integer -> StdGen -> Maybe Integer
pollard n gen = pollardX n (fst $ randomR (0, (n-1)) gen)

-- version with given x
pollardX :: Integer -> Integer -> Maybe Integer
pollardX n x
  | x >= n = Nothing  -- FAIL
  | otherwise = pollardAux n (mod (x^2 +1) n) (mod((x^2 +1)^2 +1) n)

pollardAux :: Integer -> Integer -> Integer -> Maybe Integer
pollardAux n x y
  | 1 < d && d < n  = Just d
  | d == n          = Nothing --FAIL no conclusion. Maybe run again with different x
  | otherwise       = pollardAux n (mod (x^2 +1) n) (mod((y^2 +1)^2 +1) n)
  where d = gcd n (x-y)

-- task: factorize 23802996783967
main :: IO()
main = do
  -- generator for random numbers
  g <- getStdGen
  print $ pollard 23802996783967 g

-- Outputs vary between 12347 and memory overflow.
-- As 23802996783967 `div` 12347 = 1927836461 wich is prime 
-- (checked with aks) all prime-factors are found
--
-- solution 23802996783967 = 12347 * 1927836461
