import Data.Bits

millerRabinTest2to10000 :: Integer -> Bool
millerRabinTest2to10000 n = and [millerRabinTestA n a | a <- [2..10000]]

millerRabinTestA :: Integer -> Integer -> Bool
millerRabinTestA n a
  | a > n-1   = undefined
  | even n    = False
  | b == 1 || b == (n-1) = True
  | otherwise = or [ b' == 1 || b' == (n-1) | i <-[0..(k-1)], b' <- [fastModExp b (shiftL 1 i) n]]
  where (m,k) = findMK n
        b = fastModExp a m n

-- finds m k such that:  n-1 = 2^k * m  with m odd
findMK :: Integer -> (Integer, Int)
findMK n = findMKAux (n-1) (n-1) 0

findMKAux :: Integer -> Integer -> Int -> (Integer, Int)
findMKAux origN n k
  | mod n 2 == 0 = findMKAux origN (div n 2) (k+1)
  | otherwise = (m,k)
  where m = div origN (2^k)

-- fast version of y = a^n mod x
fastModExp :: Integer -> Integer -> Integer -> Integer
fastModExp a n x = fastModExpAux (mod a x) n x 1

fastModExpAux :: Integer -> Integer -> Integer -> Integer -> Integer
fastModExpAux b n x y
  | n == 0        = y
  | n .&. 1 == 1  = fastModExpAux (mod (b*b) x) (shiftR n 1) x (mod (y*b) x)
  | otherwise     = fastModExpAux (mod (b*b) x) (shiftR n 1) x y

main :: IO()
main = do 
  putStr "millerRabinTest for 2 to 10000 = "
  print res
  putStr "number of Trues = "
  print trueses
  putStr "number of False = "
  print falses
  putStr "success rate = "
  if res == True then print $ realToFrac(trueses) / 9999 else print $ (realToFrac falses) / 9999
  where n = 225593397919
        resList = [millerRabinTestA n a | a <- [2..10000]]
        res = and resList
        trueses = length $ filter (\x -> x == True) resList
        falses = length $ filter (\x -> x == False) resList
        l = [trueses, falses]

