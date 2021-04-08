module Task3 where

import Data.Maybe
import Text.Read
import Data.Numbers.Primes

--1
data Tree a = Nil | Leaf a | Node a (Tree a) (Tree a) (Tree a)
    deriving Show

------------------------------------------------------------------------
testTree1 = Node 3 (Leaf 1) (Leaf 2) (Leaf 4)
testTree2 = Node 6 (Node 3 (Leaf 1) (Leaf 2) (Leaf 4)) Nil Nil
testTree3 = Node 6 (Node 3 (Leaf 1) (Leaf 2) (Leaf 4)) (Leaf 5) (Leaf 7)
testTree4 = Node 3 (Leaf 1) (Leaf 2) (Leaf 7)
------------------------------------------------------------------------

--2
seek :: Ord a => a -> Tree a -> Maybe a
getMaybe::(Maybe a, Maybe a) -> Maybe a

------------------------------------------------------------------------
contains :: Ord a => a -> Tree a -> Bool
contains x tree = isJust $ seek x tree
------------------------------------------------------------------------

getMaybe (fst, snd)
    | isJust fst = fst
    | isJust snd = snd
    | otherwise = Nothing

seek x Nil = Nothing
seek x (Leaf a) = if x == a then Just a else Nothing
seek x (Node v l c r)
    | x == v = Just v
    | x > v = seek x r
    | otherwise = getMaybe (seek x l, seek x c)

--3
delete :: Ord a => a -> Tree a -> Tree a
delete' :: (Ord a) => Tree a -> Tree a 
leftiest :: (Ord a) => Tree a -> a

-- Delete root
delete' (Node v Nil Nil r) = r 
delete' (Node v Nil c Nil) = c 
delete' (Node v l Nil Nil) = l 
delete' (Node v l c r) = (Node v2 l c (delete v2 r))
    where
		v2 = leftiest r

leftiest (Leaf l) = l
leftiest (Node v Nil _ _) = v
leftiest (Node v l _ _) = leftiest l

delete _ Nil = Nil
delete x (Leaf v) = if x == v then Nil else Leaf v
delete x (Node v l c r)  
	| x == v = delete' (Node v l c r)
	| x  > v = Node v l c (delete x r)
	| otherwise = Node v (delete x l) (delete x c) r

--4
add :: Ord a => a -> Tree a -> Tree a

add x Nil = Node x Nil Nil Nil
add x (Leaf v) = 
    if v == x then 
        Leaf v 
    else
        if x > v then
            Node v Nil Nil (Leaf x)
            else
                Node v (Leaf x) Nil Nil 

add x (Node v l c r)
    | v == x = Node x l c r
    | x > v = Node v l c (add x r)
    | contains x l || contains x c = Node v l c r
    | otherwise = Node v (add x l) c r

------------------------------------------------------------------------
data N = Z | S N
    deriving Show
------------------------------------------------------------------------

--8
natSum :: N -> N -> N
natSum Z x = x
natSum x Z = x
natSum x (S y) = S (natSum x y)

--9
natMult :: N -> N -> N
natMult Z _ = Z
natMult _ Z = Z
natMult x (S y) = natSum x (natMult x y)

--11
instance Functor Tree where
    fmap f Nil = Nil
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node v l c r) = Node (f v) (fmap f l) (fmap f c) (fmap f r)

testTree2Functor = fmap (+2) testTree2

--12
instance Eq N where
    (==) Z Z = True
    (==) (S _) Z = False
    (==) Z (S _) = False
    (==) (S x) (S y) = x == y

instance Ord N where
    compare Z Z = EQ
    compare (S _) Z = GT
    compare Z (S _) = LT
    compare (S x) (S y) = compare x y     