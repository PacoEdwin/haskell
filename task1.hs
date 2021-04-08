module Task1 where

import Data.Numbers.Primes

--1
f1_1 :: Int -> Int
f1_1 x = x

--2
f2_1 :: Int -> Bool
f2_1 x = isPrime x

--3
f3_1 ::  Bool -> Bool -> Int
f3_1 bool1 bool2 = (fromEnum bool1 :: Int) + (fromEnum bool2 :: Int)

--4 
-- parse array of primes to an array of pairs (p_i, a_i)
parsePrimes [] x = x
parsePrimes (x: tail) [] = parsePrimes tail [(x, 1)]
parsePrimes (x: tail1) ((fst, snd): tail2) = if x == fst then parsePrimes tail1 ((fst, snd+1) : tail2) 
                                        else parsePrimes tail1 ((x, 1):(fst, snd):tail2)
parsePrimesWrapper x = parsePrimes (primeFactors x) []

-- Implement sum of divisors
divisorSum' :: Int -> [(Int, Int)] -> Int
divisorSum' res [] = res
divisorSum' res ((p, a): tail) = divisorSum' ( res*(((p^(a+1)) - 1) `div` (p - 1)) ) tail

divisorSum x = divisorSum' 1 x
aliquotSum x = divisorSum (parsePrimesWrapper (abs x)) - (abs x)

f4_1 :: Int -> Int
f4_1 x = aliquotSum x

--5
f5_1 :: Int -> Int
f5_1 n = if f4_1 (n + 1) == n + 1 then n + 1
                        else f5_1 (n + 1)

--6
divisorSumL' :: Integer -> [(Integer, Integer)] -> Integer
divisorSumL' res [] = res
divisorSumL' res ((p, a): tail) = divisorSumL' ( res*(((p^(a+1)) - 1) `div` (p - 1)) ) tail

divisorSumL x = divisorSumL' 1 x
aliquotSumL x = divisorSumL (parsePrimesWrapper (abs x)) - (abs x)

f4_1L :: Integer -> Integer
f4_1L x = aliquotSumL x

f6_1 :: Integer -> Integer
f6_1 n = if f4_1L (n + 1) == n + 1 then n + 1
                        else f6_1 (n + 1)

--7
f7_1 :: Int -> Int -> Int
f7_1 m n
    | m == 0 = n + 1
    | m > 0 && n == 0 = f7_1 (m-1) 1
    | m > 0 && n > 0 = f7_1 (m-1) (f7_1 m (n-1))

--8
f8_1 :: Int -> Int -> Integer
f8_1 m n = toInteger $ f7_1 m n

--12
f12_1 :: Int
f12_1 = 2