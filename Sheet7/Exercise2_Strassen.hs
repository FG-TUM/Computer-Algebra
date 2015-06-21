import Data.Matrix

-- use e.g. fromList to get Matrix input
strassenMult :: Num a => Matrix a -> Matrix a -> Matrix a
strassenMult a b
  | k == 0 = a * b
  | otherwise = setSize 0 m l (joinBlocks (c11, c12, c21, c22))
  where
    m = length $ getCol 1 a
    n = length $ getCol 1 b
    l = length $ getRow 1 b
    k = head [k | k <- [0..], m <= 2^k, n <= 2^k, l <= 2^k ]
    a' = setSize 0 (2^k) (2^k) a
    b' = setSize 0 (2^k) (2^k) b
    (a11, a12, a21, a22) = splitBlocks (2^(k-1)) (2^(k-1)) a'
    (b11, b12, b21, b22) = splitBlocks (2^(k-1)) (2^(k-1)) b'
    m1 = strassenMult (a12 - a22) (b21 + b22)
    m2 = strassenMult (a11 + a22) (b11 + b22)
    m3 = strassenMult (a11 - a21) (b11 + b12)
    m4 = strassenMult (a11 + a12) b22
    m5 = strassenMult a11 (b12 - b22)
    m6 = strassenMult a22 (b21 - b11)
    m7 = strassenMult (a21 + a22) b11
    c11 = m1 + m2 - m4 + m6
    c12 = m4 + m5
    c21 = m6 + m7
    c22 = m2 - m3 + m5 - m7

main :: IO()
main = print $ strassenMult a b
  where a = fromList 2 2 [1,3,5,7]
        b = fromList 2 2 [1,2,4,9]

{-
  Output:
  (13 29)
  (33 73)
-}
