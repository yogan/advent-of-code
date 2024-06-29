module Main (main) where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  fileName <- head <$> getArgs
  input <- map parseLine . lines <$> readFile fileName
  print (part1 input)
  print (part2 input)
