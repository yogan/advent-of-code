module Main

import AoC
import Data.List
import System
import System.File

failWith : String -> IO ()
failWith msg = do
  putStrLn msg
  exitFailure

main : IO ()
main = do
  args <- getArgs
  let (file :: _) = drop 1 args | [] => failWith "Input file name is required"
  (Right content) <- readFile file
  | (Left err) => failWith ("Could not read \"" ++ file ++ "\": " ++ show err)
  let diskMap = parse content
  putStrLn $ show $ part1 diskMap
