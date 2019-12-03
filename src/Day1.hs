module Day1
    ( printTotalFuelRequired
    ) where

printTotalFuelRequired :: String -> IO ()
printTotalFuelRequired inputFile = do
  file <- readFile inputFile
  let masses = [parseMass x | x <- (lines file)]
  let fuel = sum [naiveCalculateFuel x | x <- masses]
  let actualFuel = sum [calculateFuel x | x <- masses]
  putStrLn ("D01P1: Naive total fuel requred for all modules: " ++ (show fuel))
  putStrLn ("D02P2: Actual total fuel requred for all modules: " ++ (show actualFuel))

parseMass :: String -> Integer
parseMass massString = read massString :: Integer

naiveCalculateFuel :: Integer -> Integer
naiveCalculateFuel mass = floor (fromIntegral (mass)/3) - 2

calculateFuel :: Integer -> Integer
calculateFuel mass = 
  if
    fuel > 0
  then
    fuel + (calculateFuel fuel)
  else
    0
  where fuel = naiveCalculateFuel mass
