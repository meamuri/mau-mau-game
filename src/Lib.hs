module Lib
    ( someFunc
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
solve (tableCard, playerCard) = "Yep"

someFunc :: IO ()
someFunc = (putStrLn . show) res
    where res = makeGameState $ pickCards randomCards $ getAllPairs getSuit getRank
