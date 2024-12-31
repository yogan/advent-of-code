module Main (main) where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  fileName <- head <$> getArgs
  input <- lines <$> readFile fileName
  let dims = dimensions input
  let antennas = findAntennas input
  print (part1 dims antennas)
  print (part2 dims antennas)
