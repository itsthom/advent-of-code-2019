module Day1
    ( printTotalFuelRequired
    ) where

printTotalFuelRequired :: String -> IO ()
printTotalFuelRequired inputFile = do
  file <- readFile inputFile
  let masses = map parseMass (lines file)
  let naiveFuel = sum (map naiveCalculateFuel masses)
  let actualFuel = sum (map actualCalculateFuel masses)
  putStrLn ("D01.1 -> Naive total fuel requred for all modules: " ++ (show naiveFuel))
  putStrLn ("D02.2 -> Actual total fuel requred for all modules: " ++ (show actualFuel))

parseMass :: String -> Integer
parseMass massStr = read massStr :: Integer

naiveCalculateFuel :: Integer -> Integer
naiveCalculateFuel mass = floor (fromIntegral (mass)/3) - 2

actualCalculateFuel :: Integer -> Integer
actualCalculateFuel mass = 
  if
    fuel > 0
  then
    fuel + (naiveCalculateFuel fuel)
  else
    0
  where fuel = naiveCalculateFuel mass
