module Task4 where

import System.Random

--1
readInteger :: IO Integer
readInteger = readLn

readInt :: IO Int
readInt = readLn

apb :: IO()
apb = do
    a <- readInteger
    b <- readInteger
    print (a + b)

--2
accumulate :: Integer -> IO Integer
accumulate sum = do
    a <- readInt 
    if a == 0 then return sum else accumulate (sum + a)

whileNotZero :: IO()
whileNotZero = do 
    a <- accumulate 0
    print a

--3
fakeRandom :: IO()
fakeRandom = do
    seed <- readInt
    k <- readInt
    print (take k $ randoms (mkStdGen seed) :: [Float])
