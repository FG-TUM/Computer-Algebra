-- returns the list of coefficients of the polynomial (x+a)^p
expand :: Integer -> [Integer]
expand p = scanl (\z i -> div (z * (p-i+1)) i) 1 [1..p]

-- deterministic test p for primality
aksTest :: Integer -> Bool
aksTest p
  | p < 2     = False
  | otherwise = and [mod n p == 0 | n <- init . tail $ expand p]


-- wait but ... why??
--
