---- Assignment 2 ------

import Data.List
import Text.Read
import qualified Data.Char as Char

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
        Just (messageType, remMsg) -> if length remMsg >= 2 && isNumber (head remMsg) 
                                      then LogMessage messageType (read (head remMsg)) (unwords (tail remMsg))
                                      else Unknown (unwords remMsg)
        Nothing -> Unknown message
    where msgType = parseMsgType $ words message
  
parseMsgType :: [String] -> Maybe (MessageType, [String])
parseMsgType (x:xs) = case x of "W" -> Just (Warning, xs)
                                "I" -> Just (Info, xs)
                                "E" -> if length xs > 0 && isNumber (head xs)
                                       then Just (Error (read (head xs) :: Int), tail xs)
                                       else Nothing
                                _   -> Nothing

isNumber :: String -> Bool
isNumber [] = True
isNumber (x:xs) = Char.isDigit x && isNumber xs

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insertMsg :: LogMessage -> MessageTree -> MessageTree
insertMsg (Unknown _) t = t
insertMsg msg Leaf = Node Leaf msg Leaf
insertMsg msg (Node l node r) = if getTimeStamp msg >= getTimeStamp node
                                then Node (insertMsg msg l) node r 
                                else Node l node (insertMsg msg r)   
                                                         
getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (Unknown _) = error "Unknown log message has no time stamp"
getTimeStamp (LogMessage _ ts _) = ts

build :: [LogMessage] -> MessageTree
build = foldr insertMsg Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l msg r) = (inOrder l) ++ [msg] ++ (inOrder r) 

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map (\(LogMessage _ _ s) -> s) . filter ((>50) . getError)

getError :: LogMessage -> Int
getError (LogMessage (Error n) _ _) = n
getError _ = 0

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














