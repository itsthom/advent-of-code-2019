module Day4
    ( countPasswords
    ) where

import qualified Data.List as L
import qualified Data.Text as T

countPasswords :: IO ()
countPasswords = do
  let min = 138241
  let max = 674034
  let passwords = findPasswords min max
  putStrLn ("D04.2 -> Number of valid passwords: " ++ (show (length passwords)))

findPasswords :: Int -> Int -> [Int]
findPasswords min max = 
  [x | x <- [min..max], onlyIncreases x && hasAdjacent x]
  where onlyIncreases = (\x -> L.sort (show x) == show x)

hasAdjacent :: Int -> Bool
hasAdjacent x =
  any (weirdPredicate xStr) doubles
  where xStr = show x
        doubles = [[y,y] | y <- xStr]

weirdPredicate :: String -> String -> Bool
weirdPredicate str subStr =
  L.isInfixOf subStr str && not (L.isInfixOf ((head subStr):subStr) str)
