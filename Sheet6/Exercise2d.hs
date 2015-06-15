a = 3
n = 53
g = 2

f :: Integer -> Integer
f y = case gg y of
        1 -> mod (a*y) n
        2 -> mod (y*y) n
        0 -> mod (y*g) n

ff :: (Integer, Integer) -> (Integer, Integer)
ff (alpha, beta) = case gg (mod (a^alpha * g^beta) n) of
                     1 -> (mod (alpha +1) n, beta)
                     2 -> (mod (alpha *2) n, mod (beta *2) n)
                     0 -> (alpha, mod (beta +1) n)

gg :: Integer -> Integer
gg y = mod y 3

algo :: (Integer, (Integer,Integer), Integer, (Integer, Integer)) -> (Integer, (Integer,Integer), Integer, (Integer, Integer))
algo (y, gamma, y', gamma')
  | y == y' =  (y, gamma, y', gamma')
  | otherwise = algo (f(y), ff(gamma), f(f(y')), ff(ff(gamma')))


main :: IO()
main = print $ algo (f (g^beta), ff (0,beta), f (f (g^beta)), ff (ff (0,beta)))
  where beta = 4

-- returns the values (y, gamma, y', gamma). Rest of the algorithm needs to
-- be done by hand.
-- seems to be buggy as it returns very different values for different betas
-- and so solving the congruence s*x = t (mod n) not always returns the 
-- correct result 17
