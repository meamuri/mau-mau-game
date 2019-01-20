module Lib
    ( solution
    ) where

import Control.Monad

cardsForPlayer = 5
cardsForStartGame = cardsForPlayer + 1

getSuit = ["D", "C", "S", "H"]
getRank = ["2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A"]

getAllPairs :: [String] -> [String] -> [String]
getAllPairs xs zs = liftM2 (++) xs zs

pickCards :: [Int] -> [String] -> [String]
pickCards pick cards = aggregateCards pick cards []
  where aggregateCards [] _ res = res
        aggregateCards (x:xs) cards res =
          let card = cards!!x
              cardsWithoutPicked = (take x cards) ++ (drop (x+1) cards)
          in aggregateCards xs cardsWithoutPicked (card:res)

makeGameState :: [String] -> (String, [String])
makeGameState (x:xs) = (x, xs)

randomCards = [3, 5, 6, 12, 15, 40]

solve :: (String, [String]) -> String
solve (tableCard, (x:xs))
  | head tableCard == head x || last tableCard == last x = "YES"
  | xs == [] = "NO"
  | otherwise = solve (tableCard, xs)

getList :: IO [String]
getList = do
  line <- getLine
  return $ words line

allPossiblePairs :: String
allPossiblePairs = unwords $ getAllPairs getSuit getRank

solution :: IO ()
solution = do
    putStrLn "Game must contains cards from next list:"
    putStrLn allPossiblePairs
    putStrLn "On table card:"
    cardOnTable <- getLine
    putStrLn "Some cards of user:"
    playerCards <- getList
    putStrLn $ solve (cardOnTable, playerCards)
