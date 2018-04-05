---- Assignment 3 - Code golf -----

import Data.List

skips :: [a] -> [[a]]
skips xs = [ [x | (x, y) <- zip xs (cycle [1..z]),  z == y] | z <- [1..(length xs)] ]

localMaxima :: [Int] -> [Int]
localMaxima xs
    | length xs < 3 = []
    | otherwise     = if y > x && y > z then y:localMaxima l else localMaxima l
        where (x:l@(y:z:_)) = xs
        
histogram :: [Int] -> String
histogram xs = unlines . reverse $ [['0'..'9']] ++ ["=========="] ++ hist
    where hist = map (foldl' (\acc x -> acc ++ (take (x - (length acc)) (cycle " ")) ++ "*") []) . transpose . group $ sort xs 
















