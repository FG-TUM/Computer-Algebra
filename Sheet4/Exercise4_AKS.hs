import Data.Bits
import Data.List

------------------------------------------------------------------------------------
-----------------------------------AKS-Algorithm------------------------------------
------------------------------------------------------------------------------------

-- deterministic test p for primality
-- returns false if p is divisible by any coefficient of the polynomial (x+a)^p 
-- excluding the second half of the list as it is symmetrically to the first and
-- excluding the first entry as it is one.
aksTest :: Integer -> Bool
aksTest n
  | n < 2                         = False  -- need to be excluded because of findR
  | perfectPowerTest n /= Nothing = False  -- not Nothing => n is a perfect power
  | r == n                        = True
  | mod n r == 0 && r < n         = False
  | otherwise = and [mod s n == 0 | s <- tail $ halfExpand n]
--  | otherwise = True
  where r = aksFindR n

-- only defined for n > 1
aksFindR n = aksFindRAux n (l*l) 2
  where l = (intLog2 n) +1

aksFindRAux :: Integer -> Integer -> Integer -> Integer
aksFindRAux n l2 r
  | mod n r == 0                          = r
  | and [mod (n^i) r /= 1 | i <- [1..l2]] = r
  | otherwise                             = aksFindRAux n l2 (r+1)


-- returns the first half (if length is odd including mid) of the coefficients 
-- of the polynomial (x+a)^n  as second is identical
halfExpand :: Integer -> [Integer]
halfExpand n = scanl (\z i -> div (z * (n-i+1)) i) 1 [1..(div n 2)]


------------------------------------------------------------------------------------
--------------------------------Helper-Functions------------------------------------
------------------------------------------------------------------------------------

-- fast version for y = a^n
fastExp :: Integer -> Integer -> Integer
fastExp 0 _ = 0
fastExp a 1 = a
fastExp a n = fastExpAux a n 1

fastExpAux :: Integer -> Integer -> Integer -> Integer
fastExpAux b n y
  | n == 0        = y
  | n .&. 1 == 1  = fastExpAux (b*b) (shiftR n 1) (y*b)
  | otherwise     = fastExpAux (b*b) (shiftR n 1) y

-- if n is a perfect power the fuction returns m^e = n else Nothing
perfectPowerTest :: Integer -> Maybe (Integer,Integer)
perfectPowerTest n = perfectPowerTestAux n 2 (floor $ logBase 2 (fromInteger n)) 2 n

perfectPowerTestAux :: Integer -> Integer -> Integer -> Integer -> Integer -> Maybe (Integer,Integer)
perfectPowerTestAux n e maxE m1 m2
  | e > maxE    = Nothing
  | m1 > m2     = perfectPowerTestAux n (e+1) maxE 2 n
  | mToE == n   = Just (m,e)
  | mToE >  n   = perfectPowerTestAux n e maxE m1 (m-1)
  | otherwise   = perfectPowerTestAux n e maxE (m+1) m2
  where m = div (m1 + m2) 2 -- div truncates towards neg inf
        mToE = fastExp m e

-- shortcut for the floord logarithm to base 2 for integers
intLog2 :: Integer -> Integer
intLog2 n = floor $ logBase 2 (fromInteger n)

-- returns a list of all non-perfect powers from 2 to n
listNonPerfectPowers :: Integer -> [Integer]
listNonPerfectPowers n = [2..n] \\ (nub [m^e | m <- [2..(sqrtn)], e<-[2..(intLog2 n)], (m^e) <= n])
  where sqrtn = floor (sqrt ( fromInteger n))

------Sieve of Eratosthenes--------------------------------------
-- expects two sorted infinite lists 
-- returns a sorted merged infinite list
merge :: (Ord a) => [a] -> [a] -> [a]
merge xs@(x:xt) ys@(y:yt) =
  case compare x y of
    LT -> x : (merge xt ys)
    EQ -> x : (merge xt yt)
    GT -> y : (merge xs yt)

-- expects two sorted infinite lists
-- returns the first list without all elements of the second list
diff :: (Ord a) => [a] -> [a] -> [a]
diff xs@(x:xt) ys@(y:yt) =
  case compare x y of
    LT -> x : (diff xt ys)
    EQ -> diff xt yt
    GT -> diff xs yt

-- calculates an infinite list of all odd nonprimes > 9
nonprimes :: [Integer]
nonprimes = foldr1 f $ map g $ tail primes
  where
    f (x:xt) ys = x : (merge xt ys)
    g p         = [ n * p | n <- [p, p + 2 ..]]

-- calculates an infinite list of primes
primes :: [Integer]
primes    = [2, 3, 5] ++ (diff [7, 9 ..] nonprimes)
----- End of sieve ----------------------------------------------

------------------------------------------------------------------------------------
------------------------------------task4a------------------------------------------
------------------------------------------------------------------------------------

-- finds biggest r and r/(l^5) and their respective n.
-- Output form: ((maxR, n), (maxRatio, n))
task4a :: Integer -> ((Integer, Integer), (Rational, Integer))
task4a maxN = foldl isNextBetter ((0,0),(0,0)) (listNonPerfectPowers maxN)
  where isNextBetter touple n = (maxFst (fst touple) (r,n), maxFst (snd touple) (ratio, n))
          where r = aksFindR n
                ratio = ( fromInteger r) / (fromInteger (l^5))
                l = 1 + intLog2 n
                maxFst a b = if fst a > fst b then a else b

-- execution time is horrible ~7 Minutes on an i5 @4.7GHz
-- result is ((443,182209),(9.375e-2,3))


------------------------------------------------------------------------------------
------------------------------------task4b------------------------------------------
------------------------------------------------------------------------------------

------------------------------------------------------------------------------------
--------------------------------------main------------------------------------------
------------------------------------------------------------------------------------
main :: IO()
main = print $ task4a 250000
