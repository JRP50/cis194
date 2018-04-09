---- Assignment 7 -----
import Control.Monoid

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
        deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

tag :: Monoid m => JoinList m a -> m
tag (Single x _) = x 
tag (Append x _) = x 
tag _            = mempty