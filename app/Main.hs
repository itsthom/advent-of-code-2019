module Main where

import Lib
import Day1
import Day2

main :: IO ()
main = do
  printTotalFuelRequired "input/day1"
  runIntcodeProgram "input/day2"