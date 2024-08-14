module Lib where

solve :: String -> Int -> Int
solve input times = length $ iterate lookAndSay input !! times

lookAndSay :: String -> String
lookAndSay [] = []
lookAndSay (digit : rest) =
  let (len, rest') = findConsecutiveDigits 1 digit rest
   in show len ++ [digit] ++ lookAndSay rest'

findConsecutiveDigits :: Int -> Char -> String -> (Int, String)
findConsecutiveDigits l _ [] = (l, [])
findConsecutiveDigits l d1 (d2 : rest)
  | d1 == d2 = findConsecutiveDigits (l + 1) d1 rest
  | otherwise = (l, d2 : rest)
