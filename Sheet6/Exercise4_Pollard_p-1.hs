import System.Random
import Data.Bits

-- takes a n to factorize a smoothnes bound b and a random a
pollardP1 :: Integer -> Integer -> Integer -> Integer
pollardP1 n b a = head [d | d <- ds, 1<d, d<n]
  where qs = sieveOfEratosthenes b
        ks = map (\q -> q^(floor $ logBase (fromInteger q) (fromInteger b))) qs
        as = scanl (\a k -> fastModExp a k n) a ks
        ds = map (\a -> gcd n (a-1)) as

main :: IO()
main = do
    g <- getStdGen
    print $ pollardP1 895965285150741219683 100 (fst $ randomR (2,895965285150741219683-2) g)

-- Output (for all tested "a"):
-- 99885020401

-- 895965285150741219683 / 99885020401 = 8969966483 which is prime so we
-- found all prime factors

--------------------------------------------------------------------------------------
-----------------------------------helper functions-----------------------------------
--------------------------------------------------------------------------------------

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

-- fast version of y = a^n mod x
fastModExp :: Integer -> Integer -> Integer -> Integer
fastModExp a n x = fastModExpAux (mod a x) n x 1

fastModExpAux :: Integer -> Integer -> Integer -> Integer -> Integer
fastModExpAux b n x y
  | n == 0        = y
  | n .&. 1 == 1  = fastModExpAux (mod (b*b) x) (shiftR n 1) x (mod (y*b) x)
  | otherwise     = fastModExpAux (mod (b*b) x) (shiftR n 1) x y


