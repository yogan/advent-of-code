module Lib where

part1 :: [String] -> Int
part1 = joltage 2

part2 :: [String] -> Int
part2 = joltage 12

joltage :: Int -> [String] -> Int
joltage digits banks = sum [read $ largest xs digits | xs <- banks]

largest :: (Ord a) => [a] -> Int -> [a]
largest xs 1      = [maximum xs]
largest xs digits = digit : largest rest (digits - 1)
  where
    len   = length xs - digits + 1
    digit = maximum $ take len xs
    rest  = drop 1 $ dropWhile (/= digit) xs
