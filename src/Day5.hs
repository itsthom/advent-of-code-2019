module Day5
    ( runTESTIntcodeProgram,
      execute,
      toOpcode,
      toParameterMode
    ) where

import qualified Data.List as L
import Utils

runTESTIntcodeProgram :: String -> IO ()
runTESTIntcodeProgram inputFile = do
  file <- readFile inputFile
  let intcode = toIntCode file
  let result = execute intcode 0
  putStrLn ("D05.1 -> IntCode output is: " ++ "TBD")

type Intcode = [Int]
data Opcode = Add | Multiply | Write | Read | END deriving (Enum, Eq, Show)
data ParamMode = Position | Immediate deriving (Enum, Eq, Show)
data Instruction = Instruction { index :: Int
                               , opCode :: Maybe Opcode
                               , firstParam :: (Int, ParamMode)
                               , secondParam :: (Int, ParamMode)
                               , thirdParam :: (Int, ParamMode)
                               } deriving (Show)

execute :: Intcode -> Int -> Intcode
execute code index = case opCode instruction of
  Just Add -> [1] -- TODO performAddition, etc.
  Just Multiply -> [1]
  Just Read -> [1]
  Just Write -> [1]
  Just END -> [1]
  Nothing -> [999999] -- something has gone horribly wrong
  where instruction = parseInstruction code index

parseInstruction :: Intcode -> Int -> Instruction
parseInstruction code index =
  Instruction {index = index,opCode = Just END,firstParam = (1,Position),secondParam = (1,Position),thirdParam = (1,Position)}

toOpcode :: Integral a => a -> Maybe Opcode
toOpcode x = case x of
  1 -> Just Add
  2 -> Just Multiply
  3 -> Just Read
  4 -> Just Write
  99 -> Just END
  otherwise -> Nothing

toParameterMode :: Integral a => a -> Maybe ParamMode
toParameterMode x = case x of
  0 -> Just Position
  1 -> Just Immediate
  otherwise -> Nothing

toIntCode :: String -> [Int]
toIntCode input = map read (Utils.splitStr "," input) 