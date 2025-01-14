module AoC

import Data.String

export
free : Int
free = -1

export
parse : String -> List Nat
parse = map (\c => cast (ord c - ord '0')) . unpack . trim


-- PART 1 ----------------------------------------------------------------------

export
toBlocks : List Nat -> List Int
toBlocks ds = gen ds 0 True
  where
    gen : List Nat -> Nat -> Bool -> List Int
    gen []        _   _     = []
    gen (d :: ds) fid True  = replicate d (cast fid) ++ gen ds (fid + 1) False
    gen (d :: ds) fid False = replicate d free       ++ gen ds fid       True

export
defragBlocks : List Int -> List Int
defragBlocks ds =
  let
    isFree : Int -> Bool
    isFree = (== free)

    replaceFirstFreeWith : Int -> List Int -> List Int
    replaceFirstFreeWith _   []        = []
    replaceFirstFreeWith fid (d :: ds) =
        if isFree d then
          fid :: ds else
            d :: replaceFirstFreeWith fid ds

    move : List Int -> List Int -> List Int
    move []        ds = ds
    move (b :: bs) ds = move bs (replaceFirstFreeWith b ds)

    blocks    := filter (not . isFree) ds
    toMove    := take (length $ findIndices isFree ds) $ reverse blocks
    remaining := take (minus (length ds) (length toMove)) ds
  in
    move toMove remaining

checksum : List Int -> Int
checksum ds = check 0 0 ds
  where
    check : Int -> Int -> List Int -> Int
    check acc _   []            = acc
    check acc pos (fid :: fids) = check (acc + pos * fid) (pos + 1) fids

export
part1 : List Nat -> Int
part1 = checksum . defragBlocks . toBlocks


-- PART 2 ----------------------------------------------------------------------

export
toLenBlocks : List Nat -> List (Int, Nat, Nat)
toLenBlocks ls = gen ls 0 0 True
  where
    gen : List Nat -> Nat -> Nat -> Bool -> List (Int, Nat, Nat)
    gen []        _ _ _     = []
    gen (0 :: ls) f i True  =                   gen ls (f + 1) i       False
    gen (0 :: ls) f i False =                   gen ls f       i       True
    gen (l :: ls) f i True  = (cast f, i, l) :: gen ls (f + 1) (i + l) False
    gen (l :: ls) f i False =   (free, i, l) :: gen ls f       (i + l) True

remove : Int -> List (Int, Nat, Nat) -> List (Int, Nat, Nat)
remove fid ds = filter (\(f, _, _) => f /= fid) ds

export
defragFile : List (Int, Nat, Nat) -> (Int, Nat, Nat) -> List (Int, Nat, Nat)
defragFile [] m = []
defragFile ((-1, di, dl) :: ds) (mf, mi, ml) =
  if mi < di
    -- moved beyond our own position, stop
    then (free, di, dl) :: ds
    else if dl == ml
      -- perfect fit: replace free block d with file m
      then (mf, di, ml) :: remove mf ds
      else if ml < dl
        -- space larger than file -> split: moved file, remaining space
        then (mf, di, ml) :: (free, di + ml, minus dl ml) :: remove mf ds
        -- too little space, search in remaining ds
        else (free, di, dl) :: defragFile ds (mf, mi, ml)
defragFile (d :: ds) m = d :: defragFile ds m

export
defragFiles : List (Int, Nat, Nat) -> List (Int, Nat, Nat)
defragFiles ds =
  let
    defrag : List (Int, Nat, Nat) -> List (Int, Nat, Nat) -> List (Int, Nat, Nat)
    defrag [] _         = []
    defrag ds []        = ds
    defrag ds (m :: ms) = defrag (defragFile ds m) ms
  in
    remove free $ defrag ds (reverse $ remove free ds)

checksumFiles : List (Int, Nat, Nat) -> Nat
checksumFiles ds =
  let
    check : Nat -> Nat -> Nat -> Nat -> Nat
    check acc _   _ 0 = acc
    check acc fid i l = check (acc + fid * i) fid (i + 1) (minus l 1)
  in
    sum $ map (\(fid, i, l) => check 0 (cast fid) i l) ds

export
part2 : List Nat -> Nat
part2 = checksumFiles . defragFiles . toLenBlocks
