module Day2
    ( runIntcodeProgram
    ) where

import qualified Data.List as L
import qualified Data.Text as T

runIntcodeProgram :: String -> IO ()
runIntcodeProgram inputFile = do
  file <- readFile inputFile
  let intCode = toIntCode file
  let intCode1202 = initialize intCode 12 2
  let result1202 = head (execute intCode1202 0)
  let foundInput = findInput intCode 19690720
  putStrLn ("D02.1 -> IntCode output is: " ++ (show result1202))
  putStrLn ("D02.2 -> Required input is: " ++ (show foundInput))

findInput :: [Int] -> Int -> Int
findInput code desiredOutput =
  100 * x + y
  where (x,y) = solution code desiredOutput

solution :: [Int] -> Int -> (Int,Int)
solution code desiredOutput =
  case foundSolution of
    Just foundSolution -> foundSolution
    Nothing -> (999999,999999) -- something has gone horribly wrong
  where problemSpace = [(x,y) | x <- [0..99], y <- [0..99]]
        foundSolution = L.find (testSolution code desiredOutput) problemSpace

testSolution :: [Int] -> Int -> (Int,Int) -> Bool
testSolution code desiredOutput input =
  head (execute initializedCode 0) == desiredOutput
  where initializedCode = initialize code (fst input) (snd input)

execute :: [Int] -> Int -> [Int]
execute code index
  | opCode == 1 = operate (+) code index
  | opCode == 2 = operate (*) code index
  | opCode == 99 = code
  | otherwise = execute code (index + 1)
  where opCode = code!!index

operate :: (Int -> Int -> Int) -> [Int] -> Int -> [Int]
operate operator code index = 
  execute result nextIndex
  where result = replaceAt code destination (operator num1 num2)
        num1 = code!!(code!!(index + 1))
        num2 = code!!(code!!(index + 2))
        destination = code!!(index + 3)
        nextIndex = index + 4

initialize :: [Int] -> Int -> Int -> [Int]
initialize code noun verb = replaceAt (replaceAt code 2 verb) 1 noun

replaceAt :: [Int] -> Int -> Int -> [Int]
replaceAt source index newValue =
  x ++ newValue : ys
  where (x,_:ys) = splitAt index source

toIntCode :: String -> [Int]
toIntCode input = map read (splitStr "," input) 

splitStr :: String -> String -> [String]
splitStr delimiter input =
  map T.unpack textLst
  where textLst = T.splitOn (T.pack delimiter) (T.pack input)
