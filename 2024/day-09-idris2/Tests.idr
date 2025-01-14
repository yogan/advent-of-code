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

testToLenBlocks : IO ()
testToLenBlocks = do
  test "toLenBlocks: small sample"
    (toLenBlocks $ parse "12345")
    [(0, 0, 1), (free, 1, 2), (1, 3, 3), (free, 6, 4), (2, 10, 5)]

  test "toLenBlocks: larger sample"
    (toLenBlocks $ parse "2333133121414131402")
    -- "00...111...2...333.44.5555.6666.777.888899"
    --  0 0  0  0  11  1  11 22   22   33  33   4
    --  0 2  5  8  12  5  89 12   67   12  56   0
    --  012345678901234567890123456789012345678901
    --
    --  fid   i  l
    [ (   0,  0, 2)
    , (free,  2, 3)
    , (   1,  5, 3)
    , (free,  8, 3)
    , (   2, 11, 1)
    , (free, 12, 3)
    , (   3, 15, 3)
    , (free, 18, 1)
    , (   4, 19, 2)
    , (free, 21, 1)
    , (   5, 22, 4)
    , (free, 26, 1)
    , (   6, 27, 4)
    , (free, 31, 1)
    , (   7, 32, 3)
    , (free, 35, 1)
    , (   8, 36, 4)
    , (   9, 40, 2)
    ]

testDefragBlocks : IO ()
testDefragBlocks = do
  test "defragBlocks: small sample"
    (defragBlocks (bl "0..111....22222"))
                  (bl "022111222")

  test "defragBlocks: larger sample"
    (defragBlocks (bl "00...111...2...333.44.5555.6666.777.888899"))
                  (bl "0099811188827773336446555566")

testDefragFile : IO ()
testDefragFile = do
  test "defragFile: split space into file and smaller free space"
    (defragFile
      -- "00...111................................99"
      --  0 0  0  0                               4
      --  0 2  5  8                               0
      --  012345678901234567890123456789012345678901
      --
      --  fid   i   l
      [ (   0,  0,  2)
      , (free,  2,  3)
      , (   1,  5,  3)
      , (free,  8, 32)
      , (   9, 40,  2)
      ] (   9, 40,  2))
      -- "0099.111................................"
      --  0 0 00  0
      --  0 2 45  8
      --  0123456789012345678901234567890123456789
      --
      --  fid   i   l
      [ (   0,  0,  2)
      , (   9,  2,  2)
      , (free,  4,  1)
      , (   1,  5,  3)
      , (free,  8, 32)
      ]

  test "defragFile: replace free space with file when size matches"
    (defragFile
      -- "00...111................................999"
      --  0 0  0  0                               4
      --  0 2  5  8                               0
      --  0123456789012345678901234567890123456789012
      --
      --  fid   i   l
      [ (   0,  0,  2)
      , (free,  2,  3)
      , (   1,  5,  3)
      , (free,  8, 32)
      , (   9, 40,  3)
      ] (   9, 40,  3))
      -- "00999111................................"
      --  0 0 00  0
      --  0 2 45  8
      --  0123456789012345678901234567890123456789
      --
      --  fid   i   l
      [ (   0,  0,  2)
      , (   9,  2,  3)
      , (   1,  5,  3)
      , (free,  8, 32)
      ]

testDefragFiles : IO ()
testDefragFiles = do
  test "defragFiles: reduced larger sample"
    (defragFiles
      -- "00...111................................99"
      --  0 0  0  0                               4
      --  0 2  5  8                               0
      --  012345678901234567890123456789012345678901
      --
      --  fid   i   l
      [ (   0,  0,  2)
      , (free,  2,  3)
      , (   1,  5,  3)
      , (free,  8, 32)
      , (   9, 40,  2)
      ])
      -- "0099.111................................"
      --  0 0 00  0
      --  0 2 45  8
      --  0123456789012345678901234567890123456789
      --
      --  fid   i   l
      [ (   0,  0,  2)
      , (   9,  2,  2)
      , (   1,  5,  3)
      ]

  test "defragFiles: larger sample"
    (defragFiles
      -- "00...111...2...333.44.5555.6666.777.888899"
      --  0 0  0  0  11  1  11 22   22   33  33   4
      --  0 2  5  8  12  5  89 12   67   12  56   0
      --  012345678901234567890123456789012345678901
      --
      --  fid   i  l
      [ (   0,  0, 2)
      , (free,  2, 3)
      , (   1,  5, 3)
      , (free,  8, 3)
      , (   2, 11, 1)
      , (free, 12, 3)
      , (   3, 15, 3)
      , (free, 18, 1)
      , (   4, 19, 2)
      , (free, 21, 1)
      , (   5, 22, 4)
      , (free, 26, 1)
      , (   6, 27, 4)
      , (free, 31, 1)
      , (   7, 32, 3)
      , (free, 35, 1)
      , (   8, 36, 4)
      , (   9, 40, 2)
      ])
      -- "00992111777.44.333....5555.6666.....8888.."
      --  0 0 00  0  11 11  1   2   22   3    3   4
      --  0 2 45  8  12 45  8   2   67   1    6   0
      --  012345678901234567890123456789012345678901
      --
      --  fid   i  l
      [ (   0,  0, 2)
      , (   9,  2, 2)
      , (   2,  4, 1)
      , (   1,  5, 3)
      , (   7,  8, 3)
      , (   4, 12, 2)
      , (   3, 15, 3)
      , (   5, 22, 4)
      , (   6, 27, 4)
      , (   8, 36, 4)
      ]

testPart1 : IO ()
testPart1 = test "part1: sample has the correct checksum"
  (part1 $ parse "2333133121414131402") 1928

testPart2 : IO ()
testPart2 = test "part2: sample has the correct checksum"
  (part2 $ parse "2333133121414131402") 2858

main : IO ()
main = do
  testParse
  testToBlocks
  testToLenBlocks
  testDefragBlocks
  testDefragFile
  testDefragFiles
  testPart1
  testPart2
