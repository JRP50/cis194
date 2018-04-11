{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

---- Assignment 7 -----
import Data.Monoid
import Data.List
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
        deriving (Eq, Show)

--- not sure why they get us to create +++ func instead of monoid instance
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

instance Monoid m => Monoid (JoinList m a) where
    mappend = (+++)
    mempty  = Empty

tag :: Monoid m => JoinList m a -> m
tag (Single x _) = x 
tag (Append x _ _) = x 
tag _            = mempty

tagSize :: (Sized b , Monoid b) => JoinList b a -> Int
tagSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n _ 
    | n < 0 = Nothing
indexJ n node 
    | n >= tagSize node = Nothing
indexJ 0 (Single _ a) = Just a
indexJ n (Append _ l r)
    | tagSize l > n = indexJ n l
    | otherwise     = indexJ (n - tagSize l) r
    
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

--- scrabble part

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

----------------

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

jlLine :: String -> JoinList (Score, Size) String
jlLine s = Single (scoreString s, Size 1) s

instance Buffer (JoinList (Score, Size) String) where
  toString           = unlines . jlToList
  fromString         = foldMap jlLine . lines
  line               = indexJ
  numLines           = getSize . snd . tag
  value              = getScore . fst . tag
  replaceLine n s jl = if numLines jl <= n 
                       then jl
                       else (takeJ n jl) +++ (jlLine s) +++ (dropJ n jl)
                       
reify :: (JoinList (Score, Size) String) -> (JoinList (Score, Size) String)
reify = id

main = runEditor editor . reify . fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]