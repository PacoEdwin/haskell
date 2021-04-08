module Task4 where

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
    a <- readInteger
    if a == 0 then return sum else accumulate (sum + a)

whileNotZero :: IO()
whileNotZero = do 
    a <- accumulate 0
    print a

------------------------------------------------------------------------
data FS = File String String | Directory String [FS]
    deriving Show
------------------------------------------------------------------------

--4,5
getNames :: FS -> String
getNames (File name _) = name
getNames (Directory name children) = name ++ "\n" ++ concatMap (\x -> getNames x ++ "\n") children

runFS :: FS -> IO ()
runFS fs = do
    comand <- getLine
    case comand of
        "exit" -> return ()
        
        "ls" -> do
            putStrLn (getNames fs)
            runFS fs
        
        unknown -> do
            putStrLn ("command " ++ unknown ++ " doesn't exist")
            runFS fs