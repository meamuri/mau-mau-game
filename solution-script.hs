import Control.Monad

solve :: (String, [String]) -> String
solve (tableCard, (x:xs))
  | head tableCard == head x || last tableCard == last x = "YES"
  | xs == [] = "NO"
  | otherwise = solve (tableCard, xs)

getList :: IO [String]
getList = do
  line <- getLine
  return $ words line

main :: IO ()
main = do
   cardOnTable <- getLine
   playerCards <- getList
   putStrLn $ solve (cardOnTable, playerCards)
