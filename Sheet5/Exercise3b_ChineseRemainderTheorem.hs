module ChineseRemainderTheorem where

crt :: [(Integer,Integer)] -> Integer
crt [] = 0
crt list = crtH list (mulM list)

crtH :: [(Integer,Integer)] -> Integer -> Integer
crtH [] _ = 0
crtH ((a,b) : xs) m = mod (((inverse (div m b) b) * (div m b) * a) + (crtH xs m)) m

mulM :: [(Integer,Integer)] -> Integer
mulM [] = 1
mulM ((a,b):xs) = b * mulM xs

eea :: Integer -> Integer -> (Integer, Integer, Integer)
eea a b = help a b 1 0 0 1

help :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> (Integer, Integer, Integer)
help r0 0 s0 _ t0 _ = (r0, s0, t0)
help r0 r1 s0 s1 t0 t1 = help r1 (r0 - r1*q) s1 (s0 - q*s1) t1 (t0- q*t1)
  where q = div r0 r1

--inverse of a in b
inverse :: Integer -> Integer -> Integer
inverse a b = extract (eea a b)
  where extract (_,c,_) = c
