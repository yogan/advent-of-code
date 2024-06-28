module Lib
  ( part1,
    part2,
    parseLines,
  )
where

parseLines :: [String] -> [[Int]]
parseLines = map (map read . words . map (\c -> if c == 'x' then ' ' else c))

surfaceArea :: [Int] -> Int
surfaceArea [l, w, h] = 2 * l * w + 2 * w * h + 2 * h * l
surfaceArea _ = error "Invalid input"

volume :: [Int] -> Int
volume [l, w, h] = l * w * h
volume _ = error "Invalid input"

part1 :: [[Int]] -> Int
part1 input = sum volumes
  where
    volumes = map volume input

part2 :: [[Int]] -> Int
part2 input = sum surfaceAreas
  where
    surfaceAreas = map surfaceArea input
