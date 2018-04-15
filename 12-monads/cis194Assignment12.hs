-- Assignment 12 --
import Risk
import Control.Monad.Random
import Data.List

battle :: Battlefield -> Rand StdGen Battlefield
battle b@(Battlefield att def) = liftM2 (newBattleField b) aRolls dRolls
    where 
        aRolls = nSortedDieRolls $ min 3 (att - 1)
        dRolls = nSortedDieRolls $ min 2 def

nSortedDieRolls :: Int -> Rand StdGen [DieValue]
nSortedDieRolls n = liftM (reverse . sort) . sequence $ replicate n die

newBattleField :: Battlefield -> [DieValue] -> [DieValue] -> Battlefield
newBattleField (Battlefield oldA oldD) att def 
    = Battlefield (oldA - attLost) (oldD - defLost)
        where 
            defLost = length $ filter (==True) result
            attLost = length $ filter (==False) result
            result  = zipWith (>) att def

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield att def)
    | att <= 1 || def == 0 = return b
    | otherwise            = battle b >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb b = do
    bfs <- replicateM 1000 $ invade b
    let noDefs = length . filter (==0) $ map defenders bfs
    return $ fromIntegral noDefs / 1000

main = do
  let bf1 = Battlefield 3 2
  putStrLn "Prob of attackers winning invade for 3 vs 2"
  print =<< (evalRandIO $ successProb bf1)
  let bf2 = Battlefield 5 4
  putStrLn "Prob of attackers winning invade for 5 vs 4"
  print =<< (evalRandIO $ successProb bf2)
  let bf3 = Battlefield 4 5
  putStrLn "Prob of attackers winning invade for 4 vs 5"
  print =<< (evalRandIO $ successProb bf3)