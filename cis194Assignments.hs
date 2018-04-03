----- Assignments from http://www.seas.upenn.edu/~cis194/spring13/lectures.html ---

import Data.List
import Control.Applicative
import Text.Read
import qualified Data.Char as Char


---- Assignment 1 ------
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 s _ d = [(s, d)]
hanoi n s t d = hanoi (n-1) s d t
             ++ [(s, d)]
             ++ hanoi (n-1) t s d


---- Assignment 2 ------

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

parseMessage :: String -> LogMessage
parseMessage message =
    case msgType of 
        Just (messageType, remMsg) -> if length remMsg == 2 && isNumber (head remMsg) 
                                      then LogMessage messageType (read (head remMsg)) (last remMsg)
                                      else Unknown (unwords remMsg)
        Nothing -> Unknown message
    where msgType = parseMessage' $ words message
  
parseMessage' :: [String] -> Maybe (MessageType, [String])
parseMessage' (x:xs) = case x of "W" -> Just (Warning, xs)
                                 "I" -> Just (Info, xs)
                                 "E" -> if length xs > 0 && isNumber (head xs)
                                        then Just (Error (read (head xs) :: Int), tail xs)
                                        else Nothing
                                 _   -> Nothing
         
isNumber :: String -> Bool
isNumber [] = True
isNumber (x:xs) = Char.isDigit x && isNumber xs

    
-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
testParse parse n file = take n . parse <$> readFile file

-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
testWhatWentWrong :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO [String]
testWhatWentWrong parse whatWentWrong file
  = whatWentWrong . parse <$> readFile file