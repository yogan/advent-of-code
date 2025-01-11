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

bl : String -> List Int
bl = map (\c => if c == '.' then -1 else cast (ord c - ord '0')) . unpack

testParse : IO ()
testParse = do
  test "parse: empty string gives empty list" (parse "") []

  test "parse: non-empty string is split into list of digits"
    (parse "10012390")
    [1, 0, 0, 1, 2, 3, 9, 0]

  test "parse: trailing newline is ignored"
    (parse "10012390\n")
    [1, 0, 0, 1, 2, 3, 9, 0]

testToBlocks : IO ()
testToBlocks = do
  test "toBlocks: small sample"
    (toBlocks $ parse "12345")
    (bl "0..111....22222")

  test "toBlocks: larger sample"
    (toBlocks $ parse "2333133121414131402")
    (bl "00...111...2...333.44.5555.6666.777.888899")

testDefragStep : IO ()
testDefragStep = do
  test "defragStep: small sample step 1"
    (defragStep (bl "0..111....22222"))
                (bl "02.111....2222")

  test "defragStep: small sample step 2"
    (defragStep (bl "02.111....2222"))
                (bl "022111....222")

  test "defragStep: small sample last step"
    (defragStep (bl "02211122..2"))
                (bl "022111222.")

  test "defragStep: larger sample trailing block"
    (defragStep (bl "0099811188.2...333.44.5555.6666.777.8"))
                (bl "009981118882...333.44.5555.6666.777.")

  test "defragStep: larger sample trailing space"
    (defragStep (bl "009981118882...333.44.5555.6666.777."))
                (bl "009981118882...333.44.5555.6666.777")

testDefragBlocks : IO ()
testDefragBlocks = do
  test "defragBlocks: small sample"
    (defragBlocks (bl "0..111....22222"))
                  (bl "022111222")

  test "defragBlocks: larger sample"
    (defragBlocks (bl "00...111...2...333.44.5555.6666.777.888899"))
                  (bl "0099811188827773336446555566")

testPart1 : IO ()
testPart1 = test "part1: sample has the correct checksum"
  (part1 $ parse "2333133121414131402") 1928

main : IO ()
main = do
  testParse
  testToBlocks
  testDefragStep
  testDefragBlocks
  testPart1
