module Task2 where
import Data.List
import Data.Maybe
import Text.Read
import Data.Numbers.Primes

------------------------------------------------------------------------
deleteN :: Int -> [a] -> [a]
deleteN _ []     = []
deleteN i (a:as)
   | i == 0    = as
   | otherwise = a : deleteN (i-1) as

removeVal :: Eq a => a -> [a] -> [a]
removeVal val [] = []
removeVal val (el: tail)
    | val == el = removeVal val tail
    | otherwise = el : removeVal val tail
------------------------------------------------------------------------

-- 1
antisort :: Ord a => [a] -> [a]

elemIndexLast :: Ord a => a -> [a] -> Int
elemIndexLast el x = last ( elemIndices el x)

processSingle' val (el : tail) = el : val : tail 
processSingle i x = processSingle' (x !! i) (deleteN i x)

processMultiple' fVal lVal x = (fVal : x) ++ [lVal]
processMultiple f l x = processMultiple' (x !! f) (x !! l) (deleteN (l-1) (deleteN f x)) 

antisort x 
     | fromJust( elemIndex (minimum x) x) == elemIndexLast (minimum x) x = processSingle (elemIndexLast (minimum x) x) x
     | otherwise = processMultiple (fromJust( elemIndex (minimum x) x)) (elemIndexLast (minimum x) x) x

-- 2
antiprimes :: Int -> [Integer]
antiprimes' :: Integer -> Int -> [Integer]
antiprimes' counter k
    | k == 0 = []
    | isPrime counter = antiprimes' (counter + 1) k
    | otherwise = counter : antiprimes' (counter + 1) (k-1) 

antiprimes k = antiprimes' 1 k

--3
antiunion :: Eq a => [a] -> [a] -> [a]

elem' y x = not (elem x y) 
antiunion x y = (filter (elem' y) x) ++ (filter (elem' x) y)

--4
antimerge :: Eq a => [a] -> [(Int, a)]

antimerge' val [] = (1, [])
antimerge' val (el: tail)
    | val == el = increment (antimerge' val tail)
    | otherwise = addEl el (antimerge' val tail) 
    where 
        increment (n, arr) = (n+1, arr) 
        addEl el (n, arr) = (n, el: arr)

antimerge [] = []
antimerge (a:x) = pr (antimerge' a x)
    where 
        pr (n, arr) = (n, a) : antimerge (arr) 


--5
antiintercalate :: Eq a => [a] -> [(Int, a)]

antiintercalate' [] x = x
antiintercalate' (x: tail) [] = antiintercalate' tail [(1, x)]
antiintercalate' (x: tail1) ((fst, snd): tail2) = if x == snd then antiintercalate' tail1 ((fst+1, snd) : tail2) 
                                        else antiintercalate' tail1 ((1,x):(fst, snd):tail2)
antiintercalate x = reverse (antiintercalate' x [])

--6
antiantiintercalate :: [(Int, a)] -> [a]

antiantiintercalate [] = []
antiantiintercalate ((n, el): tail)
    | n == 0 = antiantiintercalate tail
    | otherwise = el : antiantiintercalate ((n-1, el): tail)

--7
getNumberOrNot :: String -> Maybe Integer

process [] = Just ""
process (c: tail) 
    | c >= '0' && c <= '9' = Just (c : (fromJust (process tail)))
    | c == ' ' || c == '\n' = process tail
    | otherwise = error "Parsing went wrong"

failed x = Nothing
    
preProcess [] = process
preProcess (c: tail) 
    | c >= '0' && c <= '9' = preProcess tail
    | c == ' ' || c == '\n' = preProcess tail
    | otherwise = failed

getNumberOrNot x = readMaybe ((preProcess x) x)
    where 
        readMaybe s 
            | isJust s = Just (read (fromJust s) :: Integer)
            | otherwise = Nothing


--9
maybeMaybeMaybeMaybeMaybeMaybeMaybeOrNot ::Maybe (Maybe (Maybe (Maybe (Maybe (Maybe a))))) -> a -> a
maybeMaybeMaybeMaybeMaybeMaybeMaybeOrNot m a = case m of
    Nothing -> a
    (Just b) -> case b of
        Nothing -> a
        (Just c) -> case c of
            Nothing -> a
            (Just d) -> case d of
                Nothing -> a
                (Just e) -> case e of
                    Nothing -> a
                    (Just f) -> case f of
                        Nothing -> a
                        (Just g) -> g

--10
stupidTraverse :: [Maybe a] -> Maybe [(a, a, a, a)]
tuplify4 :: [Maybe a] -> (a,a,a,a)
tuplifyArray :: [Maybe a] -> [(a, a, a, a)]

tuplify4 [(Just a),(Just b),(Just c),(Just d)] = (a,b,c,d)

tuplifyArray [(Just a),(Just b),(Just c),(Just d)] = [(a,b,c,d)]
tuplifyArray x = append (splitAt 4 x)
    where
        append (x, y) = (tuplify4 x) : tuplifyArray y

stupidTraverse x = checkMod (filter isJust x)
    where
        checkMod [] = Nothing
        checkMod x
            | (length x) `mod` 4 == 0 = Just (tuplifyArray x)
            | otherwise = Nothing