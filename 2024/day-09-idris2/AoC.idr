module AoC

import Data.String

export
parse : String -> List Nat
parse = map (\c => cast (ord c - ord '0')) . unpack . trim

free : Int
free = -1

isFree : Int -> Bool
isFree = (== free)

export
toBlocks : List Nat -> List Int
toBlocks ds = gen ds 0 True
  where
    gen : List Nat -> Nat -> Bool -> List Int
    gen []        _  _      = []
    gen (d :: ds) fid True  = replicate d (cast fid) ++ gen ds (fid + 1) False
    gen (d :: ds) fid False = replicate d free       ++ gen ds fid       True

replaceFirstFreeWith : Int -> List Int -> List Int
replaceFirstFreeWith _   []        = []
replaceFirstFreeWith fid (d :: ds) =
    if isFree d then
      fid :: ds else
        d :: replaceFirstFreeWith fid ds

-- This is wildly inefficient due to reversing the list twice, but I have no
-- clue about Idris. :-/
export
defragStep : List Int -> List Int
defragStep ds =
  case reverse ds of
    (fid :: rds) => replaceFirstFreeWith fid (reverse rds)
    _            => []

export
defragBlocks : List Int -> List Int
defragBlocks ds =
  case find isFree ds of
    Just _  => defragBlocks (defragStep ds)
    Nothing => ds

checksum : List Int -> Int
checksum ds = checkRec 0 0 ds
  where
    checkRec : Int -> Int -> List Int -> Int
    checkRec acc _   []            = acc
    checkRec acc pos (fid :: fids) = checkRec (acc + pos * fid) (pos + 1) fids

export
part1 : List Nat -> Int
part1 = checksum . defragBlocks . toBlocks
