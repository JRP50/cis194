---- Assignment 7 -----
import Data.Monoid
import Sized
import Scrabble

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
        deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

tag :: Monoid m => JoinList m a -> m
tag (Single x _) = x 
tag (Append x _ _) = x 
tag _            = mempty

tagSize :: (Sized b , Monoid b) => JoinList b a -> Int
tagSize = getSize . size . tag

indexJ :: (Sized b , Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ a) = Just a
indexJ n (Append s l r)
    | currSize < n = Nothing
    | lSize >= n   = indexJ n l
    | otherwise    = indexJ (n - lSize) r
        where
            currSize = getSize . size $ s
            lSize    = tagSize l 
indexJ _ _ = Nothing
    
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n node 
    | n <= 0            = node
    | n >= tagSize node = Empty
dropJ _ s@(Single _ _)  = s
dropJ n (Append s l r )
    | n >= tagSize l    = dropJ (n - tagSize l) r
    | otherwise         = (dropJ n l) +++ r

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n node 
    | n <= 0            = Empty
    | n >= tagSize node = node
takeJ _ s@(Single _ _) = s
takeJ n (Append s l r)
    | n <= tagSize l = takeJ n l
    | otherwise      = l +++ takeJ (n - tagSize l) r

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

