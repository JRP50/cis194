---- Assignment 6 -----

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

standardFib :: Integer -> Integer -> [Integer]
standardFib 0   1    = 0 : 1 : 1 : standardFib 1 1
standardFib low high = newHigh:standardFib high newHigh
    where newHigh = low + high

fibs2 :: [Integer]
fibs2 = standardFib 0 1

fancyFib :: [Integer]
fancyFib = zipWith (+) (0:1:fancyFib) (1:fancyFib)

data Stream a = Cons a (Stream a) deriving Show