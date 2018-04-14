---- Assignment 8 - Party -----
import Data.Monoid
import Data.Tree
import Employee

instance Monoid GuestList where
    mappend (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)
    mempty = GL [] 0

glCons :: Employee -> GuestList -> GuestList
glCons e (GL l f) = GL (e:l) (f + empFun e)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node v xs) = f v (map (treeFold f) xs)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel b xs = (bestWithBoss, bestWithoutBoss)
    where 
        bestWithBoss    = glCons b (foldMap snd xs)
        bestWithoutBoss = foldMap fst xs
        
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

main :: IO ()
main = do
    file <- readFile "company.txt"
    let hierarchy = read file :: Tree Employee
        (GL l f)  = maxFun hierarchy
    putStrLn $ "Total fun: " ++ (show f)
    mapM_ putStrLn $ map empName l
        
    