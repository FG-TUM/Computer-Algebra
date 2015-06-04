module ChineseRemainderTheorem where

main :: IO()
main = print $ crt [(865, 913), (58, 589), (1168,1403)]

-- Computes CRT for x = y mod n, takes list: [(y_i, n_i)]
crt :: [(Integer,Integer)] -> Integer
crt [] = 0
crt list = crtH list (mulM list)

crtH :: [(Integer,Integer)] -> Integer -> Integer
crtH [] _ = 0
crtH ((a,b) : xs) m = mod (((inverse (div m b) b) * (div m b) * a) + (crtH xs m)) m

-- Computes product of all n_i
mulM :: [(Integer,Integer)] -> Integer
mulM [] = 1
mulM ((a,b):xs) = b * mulM xs

-- Computes gcd by Extended Euclidean Algorithm
eea :: Integer -> Integer -> (Integer, Integer, Integer)
eea a b = eeaH a b 1 0 0 1

eeaH :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> (Integer, Integer, Integer)
eeaH r0 0 s0 _ t0 _ = (r0, s0, t0)
eeaH r0 r1 s0 s1 t0 t1 = eeaH r1 (r0 - r1*q) s1 (s0 - q*s1) t1 (t0- q*t1)
  where q = div r0 r1

--inverse of a in b
inverse :: Integer -> Integer -> Integer
inverse a b = extract (eea a b)
  where extract (_,c,_) = c
