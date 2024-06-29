module Lib where

import Data.List (isInfixOf)

-- Part 1 ----------------------------------------------------------------------

part1 :: [[String]] -> Int
part1 ips = length $ filter supportsTls ips

supportsTls :: [String] -> Bool
supportsTls xs = abbaOutsideOfBrackets && not abbaWithinBackets
  where
    (odds, evens) = splitOddEven . map hasABBA $ xs
    abbaOutsideOfBrackets = or odds
    abbaWithinBackets = or evens

hasABBA :: String -> Bool
hasABBA (a : b : c : d : xs) =
  (a == d && b == c && a /= b) || hasABBA (b : c : d : xs)
hasABBA _ = False

-- Part 2 ----------------------------------------------------------------------

part2 :: [[String]] -> Int
part2 ips = length $ filter supportsSsl ips

supportsSsl :: [String] -> Bool
supportsSsl ips = any (`containsBAB` ins) abas
  where
    (outs, ins) = splitOddEven ips
    abas = concatMap findABAs outs

findABAs :: String -> [String]
findABAs (a : b : c : xs) =
  if a == c && a /= b
    then [a, b, c] : findABAs (b : c : xs)
    else findABAs (b : c : xs)
findABAs _ = []

containsBAB :: String -> [String] -> Bool
containsBAB [a, b, a'] =
  if a == a'
    then any (\s -> [b, a, b] `isInfixOf` s)
    else error "containsBAB: invalid input (need ABA)"
containsBAB _ = error "containsBAB: invalid input (need 3 characters)"

-- Helpers ---------------------------------------------------------------------

splitOddEven :: [a] -> ([a], [a])
splitOddEven [] = ([], [])
splitOddEven [o] = ([o], [])
splitOddEven (o : e : xs) = (o : os, e : es)
  where
    (os, es) = splitOddEven xs

-- Parsing ---------------------------------------------------------------------

parseLine :: String -> [String]
parseLine s = case dropWhile isBracket s of
  "" -> []
  s' -> w : parseLine s''
    where
      (w, s'') = break isBracket s'
  where
    isBracket c = c `elem` "[]"
