module Day5
    ( runTESTIntcodeProgram
    ) where

import qualified Data.List as L
import Utils

runTESTIntcodeProgram :: String -> IO ()
runTESTIntcodeProgram inputFile = do
  file <- readFile inputFile
  let intCode = toIntCode file
  let intCode1202 = initialize intCode 12 2
  let result1202 = head (execute intCode1202 0)
  putStrLn ("D05.1 -> IntCode output is: " ++ (show result1202))

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
  where result = Utils.replaceAt code destination (operator num1 num2)
        num1 = code!!(code!!(index + 1))
        num2 = code!!(code!!(index + 2))
        destination = code!!(index + 3)
        nextIndex = index + 4

initialize :: [Int] -> Int -> Int -> [Int]
initialize code noun verb = Utils.replaceAt (Utils.replaceAt code 2 verb) 1 noun

toIntCode :: String -> [Int]
toIntCode input = map read (Utils.splitStr "," input) 
