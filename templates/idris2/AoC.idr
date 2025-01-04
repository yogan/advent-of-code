module AoC

import Data.List1
import Data.String

public export
data Box = MkBox Nat Nat Nat

export
implementation Eq Box where
  MkBox a b c == MkBox x y z = a == x && b == y && c == z

export
implementation Show Box where
  show (MkBox a b c) = show a ++ "x" ++ show b ++ "x" ++ show c

listToBox : List Nat -> Maybe Box
listToBox [a, b, c] = Just (MkBox a b c)
listToBox _ = Nothing

parseLine : String -> Maybe Box
parseLine line = listToBox $ forget $ map stringToNatOrZ $ split (== 'x') line

export
parse : String -> List Box
parse input = mapMaybe id $ map parseLine $ lines input

export
volume : Box -> Nat
volume (MkBox a b c) = a * b * c

export
part1: List Box -> Nat
part1 = sum . map volume
