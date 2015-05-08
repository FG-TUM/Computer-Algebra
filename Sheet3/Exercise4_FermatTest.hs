import System.Random

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
  | mod (a^(n-1)) n /= 1 = False
  | otherwise            = True

-- fermat test with a from 2 to 1000. Only useful for n > 1000
fermatTest2to1000 :: Integer -> Bool
fermatTest2to1000 n = and $ map (\x -> fermatTestA n x) [2..1000]
