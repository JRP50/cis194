---- Assignment 1 ------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 s _ d = [(s, d)]
hanoi n s t d = hanoi (n-1) s d t
             ++ [(s, d)]
             ++ hanoi (n-1) t s d