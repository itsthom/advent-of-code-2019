module Day3
    ( printShortestDistanceToIntersection
    ) where

import qualified Data.List as L
import Utils

printShortestDistanceToIntersection :: String -> IO ()
printShortestDistanceToIntersection inputFile = do
  file <- readFile inputFile
  let [path1,path2] = map followDirections (lines file)
  let intersections = findIntersections path1 path2
  let nearest = shortestDistanceToOrigin path1 path2 intersections
  putStrLn ("D03.2 -> intersections: " ++ (show intersections))
  putStrLn ("D03.2 -> Distance to nearest intersection: " ++ (show nearest))

followDirections :: String -> [(Int,Int)]
followDirections str = 
  buildPath (map getMovement directions)
  where directions = Utils.splitStr "," str

buildPath :: [(Int,Int)] -> [(Int,Int)]
buildPath movements =
  expand (scanl acc (0,0) movements)
  where acc = (\a b -> (fst a + fst b, snd a + snd b))

expand :: [(Int,Int)] -> [(Int,Int)]
expand [] = []
expand [x] = [x]
expand path =
  if
    (fst a) - (fst b) == 0
  then
    (expandUD a b) ++ expand cs
  else
    (expandLR a b) ++ expand cs
  where a = head path
        b = head (tail path)
        cs = (tail path)

expandLR :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
expandLR a b =
  if
    fst a < fst b
  then
    (zip [(fst a + 1)..(fst b)] (repeat (snd a)))
  else
    (zip [(fst a - 1),((fst a) - 2)..(fst b)] (repeat (snd a)))

expandUD :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
expandUD a b =
  if
    snd a < snd b
  then
    (zip (repeat (fst a)) [(snd a + 1)..(snd b)])
  else
    (zip (repeat (fst a)) [(snd a - 1),((snd a) - 2)..(snd b)])

findIntersections :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
findIntersections path1 path2 =
  [x | x <- path1, x `elem` path2]

shortestDistanceToOrigin :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> Int
shortestDistanceToOrigin path1 path2 intersections =
  minimum [x | x <- trueDistances, x /= 0]
  where distances = map (\x -> (abs (fst x)) + (abs (snd x))) intersections
        trueDistances = map (\x -> (trueDistance path1 x) + (trueDistance path2 x)) intersections

trueDistance :: [(Int,Int)] -> (Int,Int) -> Int
trueDistance path element
  | index == 0 = 0
  | index > 0 = sum [1 | x <- (take (index +1) path)]
  where index = case L.elemIndex element path of Nothing -> 0
                                                 Just i -> i

getMovement :: String -> (Int,Int)
getMovement str
    | direction == 'U' = (0,distance)
    | direction == 'D' = (0,-distance)
    | direction == 'L' = (-distance,0)
    | direction == 'R' = (distance,0)
    where direction = head str
          distance = read (tail str)
