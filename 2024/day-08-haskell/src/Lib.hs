module Lib where

import qualified Data.Bifunctor
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Dimensions = (Int, Int)

type Pos = (Int, Int)

part1 :: Dimensions -> [Set.Set Pos] -> Int
part1 dims pss = Set.size $ Set.unions [allResonances dims (Set.toList ps) | ps <- pss]

allResonances :: Dimensions -> [Pos] -> Set.Set Pos
allResonances dims ps = Set.unions [resonances dims p (filter (/= p) ps) | p <- ps]

resonances :: Dimensions -> Pos -> [Pos] -> Set.Set Pos
resonances dims (r, c) others = Set.fromList $ do
  (r', c') <- others
  let (dr, dc) = (r - r', c - c')
  let (r1, c1) = (r + dr, c + dc)
  let (r2, c2) = (r' - dr, c' - dc)
  inRange dims [(r1, c1), (r2, c2)]

inRange :: Dimensions -> [Pos] -> [Pos]
inRange (rows, cols) = filter (\(r, c) -> r >= 0 && r < rows && c >= 0 && c < cols)

findAntennas :: [String] -> [Set.Set Pos]
findAntennas ls = Map.elems $ do
  Map.fromListWith Set.union . map (Data.Bifunctor.second Set.singleton) $
    [(ch, (r, c)) | (r, l) <- zip [0 ..] ls, (c, ch) <- zip [0 ..] l, ch /= '.']

dimensions :: [String] -> Dimensions
dimensions ls = (length ls, length (head ls))
