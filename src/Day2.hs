module Day2
    ( runIntcodeProgram,
      execute,
      operate,
      replaceAt,
      toIntCode,
      splitStr
    ) where

import qualified Data.Text as T

runIntcodeProgram :: String -> IO ()
runIntcodeProgram inputFile = do
  file <- readFile inputFile
  let intCode = toIntCode file
  let intCode1202 = replaceAt (replaceAt intCode 2 2) 1 12
  let result = execute intCode1202 0
  putStrLn ("D02.1 -> IntCode output is: " ++ (show result))

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
        destination = code!!(index + 3)
        num1 = code!!(code!!(index + 1))
        num2 = code!!(code!!(index + 2))
        nextIndex = index + 4

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
