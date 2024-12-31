module Main (main) where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  fileName <- head <$> getArgs
  input <- lines <$> readFile fileName
  let antennas = findAntennas input
  let dims = dimensions input
  print (part1 antennas dims)
