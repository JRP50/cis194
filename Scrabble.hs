---- Assignment 7 Scrabble-----

module Scrabble where

import Data.Monoid
import Data.Char

newtype Score = Score Int
    deriving (Eq, Ord, Show)

instance Monoid Score where
    mappend (Score n) (Score m) = Score $ m + n
    mempty = Score 0

getScore :: Score -> Int
getScore (Score x) = x

score :: Char -> Score
score char
    | c `elem` "eaionrtlsu" = Score 1
    | c `elem` "dg"         = Score 2
    | c `elem` "bcmp"       = Score 3
    | c `elem` "fhvwy"      = Score 4
    | c `elem` "k"          = Score 5
    | c `elem` "jx"         = Score 8
    | c `elem` "qz"         = Score 10
    | otherwise             = mempty
        where c = toLower char

--scoreString :: String -> Score
--scoreString = foldr (mappend . score) mempty

-- or with a lib function
scoreString :: String -> Score
scoreString = foldMap score