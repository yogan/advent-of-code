module Main (main) where

import Lib (parseLine, part1)
import System.Environment (getArgs)

main :: IO ()
main = do
  fileName <- head <$> getArgs
  input <- map parseLine . lines <$> readFile fileName
  print (part1 input)
