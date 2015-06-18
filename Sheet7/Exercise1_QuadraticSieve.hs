import Data.Bits

tfpp :: Integer -> Maybe (Integer, Integer)
tfpp n = undefined


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
