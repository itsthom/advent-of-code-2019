module Day1
    ( printTotalFuelRequired
    ) where

printTotalFuelRequired :: String -> IO ()
printTotalFuelRequired inputFile = do
  file <- readFile inputFile
  let linesOfFile = lines file
  let masses = [parseMass x | x <- linesOfFile]
  let fuel = sum [calculateFuel x | x <- masses]
  putStrLn ("Total fuel requred for all modules: " ++ (show fuel))

parseMass :: String -> Integer
parseMass massString = read massString :: Integer

calculateFuel :: Integer -> Integer
calculateFuel mass = floor (fromIntegral (mass)/3) - 2
