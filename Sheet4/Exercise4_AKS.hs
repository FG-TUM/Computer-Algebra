import Data.Bits
import Data.List

------------------------------------------------------------------------------------
-----------------------------------AKS-Algorithm------------------------------------
------------------------------------------------------------------------------------

-- deterministic test p for primality
-- returns false if p is divisible by any coefficient of the polynomial (x+1)^p 
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
-- of the polynomial (x+1)^n  as second is identical
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

-- expands (x+a)^n -(x^n +a), low endian
expandPoly :: Integer -> Integer -> [Integer]
expandPoly a n = expandPolyAux (tail $ expand n) [] a a

expandPolyAux :: [Integer] -> [Integer] -> Integer -> Integer -> [Integer]
expandPolyAux xa x ai a
  | xa == [1] = (ai - a) : x
  | otherwise = expandPolyAux (tail xa) ((head xa * (ai)):x) (ai*a) a

expand :: Integer -> [Integer]
expand n = scanl (\z i -> div (z * (n-i+1))i) 1 [1..n]

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

listNotPrimeNotPp :: Integer -> [Integer]
listNotPrimeNotPp maxN = listNonPerfectPowers maxN \\ (takeWhile (\a -> a <= maxN) primes)

listNotPrimeNotPpNotMultR maxN = listNotPrimeNotPpNotMultRAux (listNotPrimeNotPp maxN) []

listNotPrimeNotPpNotMultRAux [] l = reverse l
listNotPrimeNotPpNotMultRAux (x:xs) l
  | mod x r == 0  = listNotPrimeNotPpNotMultRAux xs l
  | otherwise     = listNotPrimeNotPpNotMultRAux xs (x:l)
  where r = aksFindR x

-- determines if a is a witness of composition of n
witnessTest :: Integer -> Integer -> Bool
witnessTest a n = not $ and [mod s n == 0 | s <- expandPoly a n]

findMinimalWitness n = head [a | a <-[1..(n-1)], witnessTest a n == True]

-- no parameter here as list values are pre computed still yields "out of memory" 
-- probable fix: propper in place modulo expansion
task4b = map findMinimalWitness theList

-- when disregarding the property of dividablenes by r the minimal witness is 1 for
-- a very long time. When taking account for r the smallest n in the list is 95477.
------------------------------------------------------------------------------------
--------------------------------------main------------------------------------------
------------------------------------------------------------------------------------
main :: IO()
main = print $ findMinimalWitness 249979

-- saved list from output of listNotPrimeNotPpNotMultR 250000
theList = [95477,96091,98587,99221,102941,103459,103603,104807,105481,106829,107917,108539,108611,109237,109783,110213,110489,111547,111649,111901,112367,112669,113803,114511,114857,115519,116339,116353,116749,116843,116939,117613,118241,118627,118829,118961,119879,120143,120979,120983,121103,121477,121757,122491,123107,123197,123463,123467,123679,124261,124573,125291,125449,125563,125701,125849,126727,126773,127117,127349,127723,128083,128759,129071,129247,129431,129551,129653,130177,130309,131513,131669,131753,132901,133667,133787,133907,134983,135199,135761,136061,136891,137317,137497,137759,137833,138553,139093,139147,140141,140561,141367,141553,141877,141923,142523,142763,142859,143959,145097,145157,145699,146087,146231,146831,146929,147167,147431,147943,148081,148613,148987,149557,149573,150103,150251,150421,150463,151139,151979,152051,152557,152849,153211,153583,153721,153773,154433,154507,154729,154967,155011,155447,155803,155989,156287,156379,156647,156701,157033,157601,158177,158497,158801,158911,159037,159101,159197,159493,159559,160477,160763,160889,161113,161191,161243,161321,161509,161587,162049,162373,162581,162733,162983,162991,163349,163439,163747,163769,164009,164063,164107,164783,164851,165073,165239,165839,166217,166343,166381,167137,167171,167477,167659,167719,167897,168019,168137,168437,168821,168989,169087,169187,169669,169921,169963,170171,170461,170771,171107,171359,171371,171389,171901,171911,171953,171961,171967,172189,172327,172699,172831,173153,173203,173323,173633,174151,174191,174283,174661,174719,174833,175031,175477,175547,175577,175793,175871,176039,176147,176279,176399,176563,176993,177097,177329,177559,177643,177773,178253,178667,178729,178861,179141,179329,179551,180049,180107,180197,180577,180589,181187,181427,181429,181451,181481,181541,181651,181663,181829,182293,182527,182731,183017,183133,183143,183257,183457,183641,183811,183913,183941,184573,184601,184619,184819,184861,185399,185617,185663,186089,186127,186331,186503,186521,186623,186803,186913,187039,187267,187619,187727,188053,188131,188549,188809,189029,189121,189209,189367,189443,189857,190087,190163,190637,190903,190933,190973,190999,191003,191117,191207,191483,191819,191941,192079,192397,192649,192911,193091,193159,193279,193339,193519,193997,194081,194111,194219,194333,194417,194449,194477,194923,194927,194947,195079,195287,195361,195667,195673,195911,196373,196487,196607,196621,196891,196967,197111,197459,197881,198001,198103,198137,198217,198547,198581,198691,198907,199183,199279,199543,199553,199613,199691,199963,200099,200219,200309,200479,200623,200701,200749,200819,200857,201277,201563,201659,201703,201793,202073,202117,202211,202379,202451,202669,203257,203447,204031,204053,204091,204109,204223,204271,204419,204863,204989,205013,205027,205039,205109,205193,205727,205729,206449,206581,206621,206711,206837,206881,206957,206989,207143,207203,207211,207313,207407,207631,207761,207887,208181,208823,208921,209051,209081,209329,209501,209557,209683,209723,209897,209999,210079,210281,210449,210677,210733,210757,210871,211103,211591,211621,211759,211763,211843,212197,212237,212603,212711,212783,212887,212983,213089,213271,213331,213377,213419,213443,213793,213907,214099,214271,214289,214777,214793,215041,215069,215071,215221,215287,215333,215429,215549,215629,215651,215741,215759,216067,216221,216409,216673,216691,216793,216941,217159,217513,217631,217799,217801,217913,217927,218299,218507,218663,218683,218693,218903,218951,218957,219007,219061,219137,219341,219347,219379,219833,220061,220067,220183,220219,220397,220459,220567,220817,220819,220991,221057,221129,221189,221269,221341,221503,221777,222119,222221,222427,222473,222559,222743,222757,222769,222829,223357,223427,223451,223511,223693,223723,224051,224173,224387,224407,224453,224507,224509,224551,224747,224821,224971,225413,225481,225487,225593,225763,225803,225847,225893,226351,226411,226439,226459,226679,226687,226801,226979,227021,227119,227173,227333,227429,227761,227779,227813,227897,228043,228149,228169,228343,228391,228541,228649,228719,228971,229069,229193,229297,229367,229417,229597,229871,229991,230039,230053,230119,230141,230183,230267,230287,230509,230677,230803,231037,231377,231577,231689,231883,232273,232327,232481,232613,232627,232721,232889,233011,233033,233171,233273,233383,233531,233539,233789,233843,233929,234253,234497,234601,234649,234779,234827,234877,234901,234919,235189,235247,235363,235387,235421,235667,235757,235897,235981,235993,236123,236273,236311,236581,236851,237001,237023,237077,237299,237449,237499,237569,237703,237793,237803,238097,238411,238457,238597,238981,239011,239021,239093,239117,239149,239249,239549,239651,239663,239839,240013,240067,240083,240133,240181,240199,240391,240791,240937,240979,241001,241103,241181,241223,241331,241607,241673,241763,241853,242149,242303,242321,242537,242587,242653,242909,242917,242939,243013,243043,243247,243307,243361,243407,243569,243697,243779,243811,243923,244241,244523,244649,244949,244961,244991,244999,245009,245197,245213,245239,245459,245503,245603,245743,245807,245809,245813,245953,246023,246101,246269,246377,246553,246751,246973,246991,247127,247157,247237,247243,247417,247441,247487,247801,247883,248069,248219,248263,248459,248467,248687,249349,249401,249409,249493,249559,249653,249761,249791,249841,249919,249979]
