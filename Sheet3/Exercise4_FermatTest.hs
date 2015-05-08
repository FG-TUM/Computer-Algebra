import System.Random
import Data.Bits

-- fermat test with random a
fermatTestRand :: Integer -> StdGen -> Bool
fermatTestRand n gen
  | even n                      = False
  | (fastModExp a (n-1) n) /= 1 = False
  | otherwise                   = True
  where (a, _) = randomR (2, n-1) gen

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

-- UNUSED
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

