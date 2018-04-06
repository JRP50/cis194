{-# LANGUAGE FlexibleInstances #-}

---- Assignment 5 -----
import Parser
import StackVM

-- commented out for last exercise
--data ExprT = Lit Integer
--           | Add ExprT ExprT
--           | Mul ExprT ExprT
--    deriving (Show, Eq)

-- eval :: ExprT -> Integer
-- eval (Lit x)   = x
-- eval (Add x y) = (eval x) + (eval y)
-- eval (Mul x y) = (eval x) * (eval y)

-- evalStr :: String -> Maybe Integer
-- evalStr s = do
    -- parsedExpr <- parseExp Lit Add Mul s
    -- return $ eval parsedExpr
    
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a
    
--instance Expr ExprT where
--    lit = Lit
--    add = Add
--    mul = Mul

--reify :: ExprT -> ExprT
--reify = id

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)


instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)
    
instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)

instance Expr MinMax where
    lit                       = MinMax
    add (MinMax x) (MinMax y) = MinMax $ max x y
    mul (MinMax x) (MinMax y) = MinMax $ min x y

instance Expr Mod7 where
    lit                   = Mod7
    add (Mod7 x) (Mod7 y) = Mod7 $ x + y
    mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7
    
reify :: Program -> Program
reify = id

-- doesn't take into account bodmas
instance Expr Program where
    lit = (:[]) . PushI
    add a b = a ++ b ++ [Add]
    mul a b = a ++ b ++ [Mul]

eval :: Program -> Maybe Integer
eval p = case stackVM p of
    (Right (IVal x)) -> return x
    _                -> Nothing
    
evalStr :: String -> Maybe Integer
evalStr s = parseExp lit add mul s >>= \parsedExpr -> eval parsedExpr