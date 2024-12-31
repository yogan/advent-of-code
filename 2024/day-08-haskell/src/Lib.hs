module Lib where

import qualified Data.Bifunctor
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Dimensions = (Int, Int)

type Pos = (Int, Int)

part1 :: Dimensions -> [Set.Set Pos] -> Int
part1 = impact resonances

part2 :: Dimensions -> [Set.Set Pos] -> Int
part2 = impact harmonics

impact :: (Dimensions -> Pos -> [Pos] -> Set.Set Pos) -> Dimensions -> [Set.Set Pos] -> Int
impact fn dims pss = Set.size $ Set.unions [antinodes dims (Set.toList ps) fn | ps <- pss]

antinodes :: Dimensions -> [Pos] -> (Dimensions -> Pos -> [Pos] -> Set.Set Pos) -> Set.Set Pos
antinodes dims ps fn = Set.unions [fn dims p (filter (/= p) ps) | p <- ps]

resonances :: Dimensions -> Pos -> [Pos] -> Set.Set Pos
resonances dims (r1, c1) others = Set.fromList $ do
  (r2, c2) <- others
  let (dr, dc) = (r1 - r2, c1 - c2)
  filter (isInRange dims) [(r1 + dr, c1 + dc), (r2 - dr, c2 - dc)]

harmonics :: Dimensions -> Pos -> [Pos] -> Set.Set Pos
harmonics dims (r1, c1) others = Set.fromList $ do
  (r2, c2) <- others
  let (dr, dc) = (r1 - r2, c1 - c2)
  let fwd = takeWhile (isInRange dims) [(r1 + i * dr, c1 + i * dc) | i <- [1 ..]]
  let bwd = takeWhile (isInRange dims) [(r2 - i * dr, c2 - i * dc) | i <- [1 ..]]
  fwd ++ [(r1, c1)] ++ bwd

isInRange :: Dimensions -> Pos -> Bool
isInRange (rows, cols) (r, c) = r >= 0 && r < rows && c >= 0 && c < cols

findAntennas :: [String] -> [Set.Set Pos]
findAntennas ls = Map.elems $ do
  Map.fromListWith Set.union . map (Data.Bifunctor.second Set.singleton) $
    [(ch, (r, c)) | (r, l) <- zip [0 ..] ls, (c, ch) <- zip [0 ..] l, ch /= '.']

dimensions :: [String] -> Dimensions
dimensions ls = (length ls, length (head ls))
