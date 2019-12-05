module Day3
    ( printNearestIntersection,
      followDirections,
      buildPath,
      expand,
      expandLR,
      expandUD,
      findIntersections,
      getMovement
    ) where

import Utils

printNearestIntersection :: String -> IO ()
printNearestIntersection inputFile = do
  file <- readFile inputFile
  -- let file' = "R8,U5,L5,D3\nU7,R6,D4,L4"
  let [path1,path2] = map followDirections (lines file)
  let intersections = findIntersections path1 path2
  let nearest = shortestDistanceToOrigin intersections
  putStrLn ("D03.1 -> Distance to nearest intersection: " ++ (show nearest))

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
    (expandUD a b) ++ expand cs -- up/down doesn't work
  else
    (expandLR a b) ++ expand cs -- L/R does work
  where a = head path
        b = head (tail path)
        cs = (tail path)

expandLR :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
expandLR a b =
  if
    fst a < fst b
  then
    (zip [(fst a)..(fst b)] (repeat (snd a)))
  else
    (zip [(fst a),((fst a) - 1)..(fst b)] (repeat (snd a)))

expandUD :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
expandUD a b =
  if
    snd a < snd b
  then
    (zip (repeat (fst a)) [(snd a)..(snd b)])
  else
    (zip (repeat (fst a)) [(snd a),((snd a) - 1)..(snd b)])

findIntersections :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
findIntersections path1 path2 =
  [x | x <- path1, x `elem` path2]

shortestDistanceToOrigin :: [(Int,Int)] -> Int
shortestDistanceToOrigin intersections =
  minimum [x | x <- distances, x /= 0]
  where distances = map (\x -> (abs (fst x)) + (abs (snd x))) intersections

getMovement :: String -> (Int,Int)
getMovement str
    | direction == 'U' = (0,distance)
    | direction == 'D' = (0,-distance)
    | direction == 'L' = (-distance,0)
    | direction == 'R' = (distance,0)
    where direction = head str
          distance = read (tail str)
