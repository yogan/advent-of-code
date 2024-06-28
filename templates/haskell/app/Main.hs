module Main (main) where

import Lib (parseLines, part1, part2)
import System.Environment (getArgs)

main :: IO ()
main = do
  fileName <- head <$> getArgs
  input <- parseLines . lines <$> readFile fileName
  print (part1 input)
  print (part2 input)
