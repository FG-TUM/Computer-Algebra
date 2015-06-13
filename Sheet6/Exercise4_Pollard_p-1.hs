import System.Random

-- takes a n to factorize a smoothnes bound b and a random a
pollardP1 :: Integer -> Integer -> Integer -> Integer
pollardP1 n b a = undefined
  where primes = sieveOfEratosthenes b

----------------------------------------------------------------------------------------------
---------------------------------------Helper functions---------------------------------------
----------------------------------------------------------------------------------------------

-- returns all primes smaller n
sieveOfEratosthenes :: Integer -> [Integer]
sieveOfEratosthenes n = takeWhile (<= n) unboundSieve

unboundSieve :: [Integer]
unboundSieve = 2 : sieve [3..] 4 unboundSieve
  where sieve (x:xs) q (p:t)
          | x < q     = x : sieve xs q (p:t)
          | otherwise = sieve (minus xs [q, q+p..]) (head t^2) t
          where minus a@(x:xs) b@(y:ys) = case compare x y of
                  LT -> x : minus xs b
                  EQ ->     minus xs ys
                  GT ->     minus a  ys
                minus a b = a
