module Lib where

solve :: String -> Int -> Int
solve input times = length $ iterate lookAndSay input !! times

lookAndSay :: String -> String
lookAndSay [] = []
lookAndSay (d : rest) = show l ++ [d] ++ lookAndSay rest'
  where
    (l, rest') = findConsecutiveDigits 1 d rest

findConsecutiveDigits :: Int -> Char -> String -> (Int, String)
findConsecutiveDigits l _ [] = (l, [])
findConsecutiveDigits l d1 (d2 : rest)
  | d1 == d2 = findConsecutiveDigits (l + 1) d1 rest
  | otherwise = (l, d2 : rest)
