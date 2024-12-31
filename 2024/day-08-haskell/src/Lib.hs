module Lib where

import qualified Data.Bifunctor
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Dimensions = (Int, Int)

type Pos = (Int, Int)

type AntennaPositions = Map.Map Char (Set.Set Pos)

part1 :: AntennaPositions -> Dimensions -> Int
part1 as dims =
  Set.size $
    Set.unions
      [allResonances (Set.toList ps) dims | (_, ps) <- Map.toList as]

allResonances :: [Pos] -> Dimensions -> Set.Set Pos
allResonances ps dims = Set.unions [resonances p (filter (/= p) ps) dims | p <- ps]

resonances :: Pos -> [Pos] -> Dimensions -> Set.Set Pos
resonances (r, c) others (rows, cols) = Set.fromList $ do
  (r', c') <- others
  let (dr, dc) = (r - r', c - c')
  let (r1, c1) = (r + dr, c + dc)
  let (r2, c2) = (r' - dr, c' - dc)
  inRange [(r1, c1), (r2, c2)] (rows, cols)

inRange :: [Pos] -> Dimensions -> [Pos]
inRange ps (rows, cols) = do
  (r, c) <- ps
  ([(r, c) | r >= 0 && r < rows && c >= 0 && c < cols])

findAntennas :: [String] -> AntennaPositions
findAntennas ls =
  Map.fromListWith Set.union . map (Data.Bifunctor.second Set.singleton) $
    [ (ch, (r, c)) | (r, l) <- zip [0 ..] ls, (c, ch) <- zip [0 ..] l, ch /= '.'
    ]

dimensions :: [String] -> Dimensions
dimensions ls = (length ls, length (head ls))
