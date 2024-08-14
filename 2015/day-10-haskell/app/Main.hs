module Main (main) where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  fileName <- head <$> getArgs
  input <- readFile fileName
  let inputWithoutNewLine = init input
  print (solve inputWithoutNewLine 40)
  print (solve inputWithoutNewLine 50)
