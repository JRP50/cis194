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

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x y) = x : streamToList y

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x y) =  Cons (f x) (streamMap f y)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f newSeed)
    where newSeed = f seed

nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- come back to ruler, need to interleave 0's with multiples of 2 in the form
-- 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5, etc. 
-- so the algo is something like take list, add n+1 to the end, append original list to the end, and add n+2 and repeat
