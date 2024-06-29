module Lib
  ( part1,
    isValid,
    splitOddEven,
    hasABBA,
    parseLine,
  )
where

part1 :: [[String]] -> Int
part1 input = length $ filter isValid input

isValid :: [String] -> Bool
isValid xs = abbaOutsideOfBrackets && not abbaWithinBackets
  where
    (odds, evens) = splitOddEven . map hasABBA $ xs
    abbaOutsideOfBrackets = or odds
    abbaWithinBackets = or evens

splitOddEven :: [a] -> ([a], [a])
splitOddEven [] = ([], [])
splitOddEven [o] = ([o], [])
splitOddEven (o : e : xs) = (o : os, e : es)
  where
    (os, es) = splitOddEven xs

hasABBA :: String -> Bool
hasABBA (a : b : c : d : xs) =
  (a == d && b == c && a /= b) || hasABBA (b : c : d : xs)
hasABBA _ = False

parseLine :: String -> [String]
parseLine s = case dropWhile isBracket s of
  "" -> []
  s' -> w : parseLine s''
    where
      (w, s'') = break isBracket s'
  where
    isBracket c = c `elem` "[]"
