--import System.Random
import Data.Bits

-- fermat test with given a
fermatTestA :: Integer -> Integer -> Bool
fermatTestA n a
  | even n                      = False
  | (fastModExp a (n-1) n) /= 1 = False
  | otherwise                   = True

-- fermat test with a from 2 to 1000. Only useful for n > 1000
fermatTest2to1000 :: Integer -> Bool
fermatTest2to1000 n = and $ map (\x -> fermatTestA n x) [2..1000]

-- fermat test with a from 2 to n (exclusive)
fermatTest2toN :: Integer -> Bool
fermatTest2toN n = and $ map (\x -> fermatTestA n x) [2..(n-1)]

-- fast version of y = a^n mod x
fastModExp :: Integer -> Integer -> Integer -> Integer
fastModExp a n x = fastModExpAux (mod a x) n x 1

fastModExpAux :: Integer -> Integer -> Integer -> Integer -> Integer
fastModExpAux b n x y
  | n == 0        = y
  | n .&. 1 == 1  = fastModExpAux (mod (b*b) x) (shiftR n 1) x (mod (y*b) x)
  | otherwise     = fastModExpAux (mod (b*b) x) (shiftR n 1) x y

main :: IO()
--main = print $ fermatTest2toN 306450443005841
main = print $ fermatTest2to1000 306450443005841


-- Observations:
-- The naive implementation of the Fermat test is pretty slow for large n
-- due to the big exonentiation. Even using the fast exponentiation algorithm
-- from the lecture (fastExp) doesn't solve the problem as the numbers get
-- to big (>6KB).
-- The problem can be solved by a small trick. As we are not interested in the
-- result of the exponentiation but in in the modulo we can directly calculate
-- the modulo of the exponentiation. This is possible by exploiting the "rule"
-- a*b mod c = ((a mod c) * (b mod c)) mod c
-- This combined with the fastExp algorithm yields a pretty fast modulo
-- exponentiation (fastModExp) that solves the given problem even for a={2..(n-1)}
-- in "not measurable short" time. The laziness of Haskell also seems to speed up
-- the function fermatTest2toN as it might not do further computations as soon
-- as it gets one "False" which happens first at a = 11633. This tells us that
-- 306450443005841 is not a prime number, but the Fermat test for a={2..1000}
-- will not come to this conclusion. Further analysis shows that it is
-- a Carmichael number.



-- everything from here on is UNUSED. It only illustrates the evolution of our code.

-- fermat test with random a; for this function import System.Random
--fermatTestRand :: Integer -> StdGen -> Bool
--fermatTestRand n gen = fermatTestA n a
--  where (a, _) = randomR (2, n-1) gen

-- fast version for y = a^n
fastExp :: Integer -> Integer -> Integer
fastExp 0 _ = 0
fastExp _ 0 = 1
fastExp a 1 = a
fastExp a n = fastExpAux a n 1

fastExpAux :: Integer -> Integer -> Integer -> Integer
fastExpAux b n y
  | n == 0        = y
  | n .&. 1 == 1  = fastExpAux (b*b) (shiftR n 1) (y*b)
  | otherwise     = fastExpAux (b*b) (shiftR n 1) y

-- returns y = a^n mod x
modExp :: Integer -> Integer -> Integer -> Integer
modExp a n x = modExpAux (mod a x) n x 1

modExpAux :: Integer -> Integer -> Integer -> Integer -> Integer
modExpAux a n x y
  | n == 0    = y
  | otherwise = modExpAux a (n-1) x (mod (a*y) x)

