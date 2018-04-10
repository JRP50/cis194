---- Assignment 4 ------

import Data.List 
import Control.Applicative

-- theirs
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' [] = 1
fun1' xs = product . map (\x -> x - 2) $ filter even xs

data Tree a = Leaf 
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: (Eq a) => [a] -> Tree a
foldTree = foldr insertInTree Leaf

-- not correct
insertInTree :: (Eq a) => a -> Tree a -> Tree a
insertInTree x Leaf = Node 0 Leaf x Leaf
insertInTree x (Node depth lTree val rTree)
    | x == val = Node depth lTree val rTree
    | ld <= rd = Node (depth + 1) (insertInTree x lTree) val rTree
    | ld > rd  = Node (depth + 1) lTree val (insertInTree x rTree)
        where ld = treeDepth lTree
              rd = treeDepth rTree

treeDepth :: Tree a -> Integer
treeDepth Leaf = 0
treeDepth (Node d _ _ _) = d

-- with point free
xor :: [Bool] -> Bool
xor = odd . length . filter (==True)

-- with fold
xor' :: [Bool] -> Bool
xor' = foldr (\x acc -> if x == True then not acc else acc) False

---- Sieve of Sundaram ----
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys ]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+) 1 . (*) 2) $ [1..n] \\ sieve
    where sieve = filter (<= n) [ i+j+2*i*j | (i, j) <- cartProd [1..n] [1..n], i <= j ]
    
-- improvements from irc
sieveSundaram' :: Integer -> [Integer]
sieveSundaram' n = map (\x -> x*2+1) $ [1..n] \\ sieve
    where
        sieve    = filter (<= n) nums
        nums     = [ i+j+2*i*j | (i, j) <- cartProd, i <= j]
        cartProd = liftA2 (,) [1..n] [1..n]
        
--- another take from irc - my favorite
sieveSundaram' :: Integer -> [Integer]
sieveSundaram' n = [ x * 2 + 1 | x <- allNums \\ sieve ]
    where
        allNums = [1..n]
        sieve   = do
            (i, j) <- LiftA2 (,) allNums allNums
            guard (i <= j)
            let v = i + j + 2 * i * j
            guard (v <= n)
            pure v
    