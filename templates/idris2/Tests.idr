module Tests

import AoC
import System

test : (Show a, Eq a) => String -> a -> a -> IO ()
test desc act exp = if act == exp
  then putStrLn $ "✅ " ++ desc
  else do
    putStrLn $ "❌ " ++ desc
    putStrLn $ "    Expected: " ++ show exp
    putStrLn $ "    Got:      " ++ show act
    exitFailure

testParse : IO ()
testParse = do
  test "parse: empty input gives empty list" (parse "") []

  test "parse: two valid lines of box dimensions are parsed"
    (parse "1x2x3\n4x5x6")
    [MkBox 1 2 3, MkBox 4 5 6]

  test "parse: invalid lines are ignored"
    (parse "1x2x3\nxxx\nAAAAAAAAA\n4x5x6")
    [MkBox 1 2 3, MkBox 4 5 6]

testVolume : IO ()
testVolume = test "volume: box 2x3x4 gives volume 24"
  (volume (MkBox 2 3 4)) 24

testPart1 : IO ()
testPart1 = test "part1: sum of volumes of two boxes"
  (part1 [MkBox 1 2 3, MkBox 1 1 1]) 7

main : IO ()
main = do
  testParse
  testVolume
  testPart1
