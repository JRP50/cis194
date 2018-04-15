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

