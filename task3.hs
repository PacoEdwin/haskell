module Task3 where
import Data.List
import Data.Maybe
import Text.Read
import Data.Numbers.Primes

--1
data Tree a = Leaf a | Node a (Tree a) (Tree a) (Tree a)
    deriving Show

testTree1 = Node 3 (Leaf 1) (Leaf 2) (Leaf 4)
testTree2 = Node 6 (Node 3 (Leaf 1) (Leaf 2) (Leaf 4)) -- (Leaf 5) (Leaf 7)

-- instance Functir Tree where
--     fmap

--2
contains :: Ord a => a -> Tree a -> Bool

contains x (Leaf a) = x == a
contains x (Node v l c r)
    | x == v = True 
    | x > v = contains x r
    | otherwise = contains x l || contains x c


seek :: Ord a => a -> Tree a -> Maybe a

getMaybe::(Maybe a, Maybe a) -> Maybe a
getMaybe (fst, snd)
    | isJust fst = fst
    | isJust snd = snd
    | otherwise = Nothing

seek x (Leaf a) = if x == a then Just a else Nothing
seek x (Node v l c r)
    | x == v = Just v
    | x > v = seek x r
    | otherwise = getMaybe (seek x l, seek x c)