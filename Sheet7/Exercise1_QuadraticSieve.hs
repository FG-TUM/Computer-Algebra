--spoda
tfpp :: Integer -> Maybe (Integer, Integer)
tfpp n = 






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
