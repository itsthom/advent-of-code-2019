module Day8
    ( testImage
    , toImage
    , findLayer
    , onesByTwos
    , countChar
    ) where

import qualified Data.List as L
import qualified Data.List.Split as LS
import Utils

(width, height) = (25,6)

testImage :: String -> IO ()
testImage input = do
  file <- readFile input
  let layer = findLayer $ toImage $ filter (\x -> elem x ['0'..'9']) file
  let answer = onesByTwos layer
  putStrLn ("D08.1 -> (#1s * #2s) in fewest-0 layer: " ++ show answer)

type Image = [Layer]
type Layer = [Char]

toImage :: String -> Image
toImage str = LS.chunksOf (width*height) str

findLayer :: Image -> Layer
findLayer img = L.minimumBy f img
  where f = (\x y -> compare (countChar '0' x) (countChar '0' y))

onesByTwos :: Layer -> Int
onesByTwos layer = countChar '1' layer * countChar '2' layer

countChar :: Char -> Layer -> Int
countChar c layer = sum $ map (\x -> if x == c then 1 else 0) layer