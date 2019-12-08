module Day8
    ( printImage
    ) where

import qualified Data.List as L
import qualified Data.List.Split as LS
import Utils

(width, height) = (25,6)

printImage :: String -> IO ()
printImage input = do
  file <- readFile input
  let image = toImage $ filter (\x -> elem x ['0'..'9']) file
  let answer1 = onesByTwos $ findLayer image
  putStrLn ("D08.1 -> (#1s * #2s) in fewest-0 layer: " ++ show answer1)
  putStrLn "D08.2 -> Image:"
  draw image

type Image = [Layer]
type Layer = [Char]

draw :: Image -> IO ()
draw image = do
  let surface = foldr f (replicate (width*height) ' ') image
  putStr $ L.intercalate "\n" $ LS.chunksOf width surface
  where f = (\x prev -> (zipWith addLayerValue prev x))

addLayerValue :: Char -> Char -> Char
addLayerValue lower upper = case upper of
  '0' -> ' '
  '1' -> '@'
  '2' -> lower
  otherwise -> lower

toImage :: String -> Image
toImage str = LS.chunksOf (width*height) str

findLayer :: Image -> Layer
findLayer img = L.minimumBy f img
  where f = (\x y -> compare (countChar '0' x) (countChar '0' y))

onesByTwos :: Layer -> Int
onesByTwos layer = countChar '1' layer * countChar '2' layer

countChar :: Char -> Layer -> Int
countChar c layer = sum $ map (\x -> if x == c then 1 else 0) layer