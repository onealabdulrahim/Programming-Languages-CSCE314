-- import Data.Numbers.Primes
-- import Data.Numbers
-- import Data.List

{-

   Goldbach's other conjecture: Christian Goldbach once proposed that
   every odd composite number can be written as the sum of a prime and
   twice a square.

-}

-- primes :: [Integer]
primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p > 0 ]
    
gold0 :: Int -> [Int]
gold0 n = [ x | 
                x <- [1..n], y <- (take n primes), z <- [1..n],
               (x == (y + (2*(z^2)))), (odd x)]

gs :: [Int]
gs = [9, 15, 21, 25, 27, 33]

gold33 = gold0 33

all_elems_inlist es gs =
   if (length es <= 0) then True
      else
        ((elem (head es) gs) && (all_elems_inlist (tail es) gs))

found = all_elems_inlist gs gold33

divides :: Int -> Int -> Bool
divides   n p = n `mod` p == 0

nodivides :: Int -> Int -> Bool
nodivides n p = not (divides n p)

primesN :: Int -> [Int]
primesN n   = takeWhile (\p -> p <= n) primes

is_Prime     :: Int -> Bool
is_Prime   n = (elem n (primesN n))

all_OddComps :: [Int]
all_OddComps   = filter (\n -> not (is_Prime n)) [3..]

is_goldbach       :: Int -> Int -> Int -> Bool
is_goldbach n p i =
   (odd n) && (is_Prime p) && (n == p + (2*(i^2)))

gen_goldbach_prim :: Int -> [Int] -> Int -> [(Int,Int,Int)]
gen_goldbach_prim n []     i = []
gen_goldbach_prim n (p:ps) i =
  (((p + (2*(i^2))), p, i):(gen_goldbach_prim n ps i))

gen_goldbach_primes :: Int -> [Int] -> [Int] -> [(Int,Int,Int)]
gen_goldbach_primes n ps     []     = []
gen_goldbach_primes n ps     (i:il) =
   ((gen_goldbach_prim n ps i) ++ (gen_goldbach_primes n ps il))

gb01 = gen_goldbach_primes 1 (take 12 primes) [1..3]

filter_goldbach :: [(Int,Int,Int)] -> [(Int,Int,Int)]
filter_goldbach []     = []
filter_goldbach (g:gs) =
   if (is_goldbach n p i) then
     ((n,p,i):(filter_goldbach gs))
   else
     (filter_goldbach gs)
   where (n,p,i) = g

gb02 = filter_goldbach gb01

get_fst :: [(Int, Int, Int)] -> [Int]
get_fst [] = []
get_fst ((a,b,c):ts) = (a:(get_fst ts))

gs2 = get_fst gb02
found2 = all_elems_inlist gs gs2
found3 = all_elems_inlist gold33 gs2

primesSqr n   = takeWhile (\p -> p^2 <= n) primes

s_CompOdds :: [Int]
s_CompOdds = filter (\n -> not (is_Prime n)) [3, 5..]

is_SqrGoldNum :: Int -> Bool
is_SqrGoldNum n = any is_Prime (takeWhile (>0) (map (\i -> n - 2*i*i) [1..]))

goldbachNumList n = filter is_SqrGoldNum [1..n]

goldbachNumList33 = goldbachNumList 33

found4 = all_elems_inlist goldbachNumList33 gold33

goldbach2_violated :: Int
goldbach2_violated = head (filter (\n -> not (is_SqrGoldNum n)) s_CompOdds)
