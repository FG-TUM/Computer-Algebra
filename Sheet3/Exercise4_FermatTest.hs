import System.Random
import Data.Bits

-- fermat test with random a
fermatTestRand :: Integer -> StdGen -> Bool
fermatTestRand n gen
  | even n               = False
  | mod (a^(n-1)) n /= 1 = False
  | otherwise            = True
  where (a, _) = randomR (2, n-1) gen

-- fermat test with given a
fermatTestA :: Integer -> Integer -> Bool
fermatTestA n a
  | even n               = False
  | mod (fastExp a (n-1)) n /= 1 = False
  | otherwise            = True

-- fermat test with a from 2 to 1000. Only useful for n > 1000
fermatTest2to1000 :: Integer -> Bool
fermatTest2to1000 n = and $ map (\x -> fermatTestA n x) [2..1000]

-- fast version for a^n
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

main = print $ fermatTestA 306450443005841 1000
