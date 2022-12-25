import Control.Monad (when)
import Data.List (find)
import System.Environment (getArgs)

fromSnafu :: String -> Int
fromSnafu s =
  let digits = map fromSnafuDigit s
      pairs = zip (reverse digits) (map (5 ^) [0 ..])
      values = map (uncurry (*)) pairs
   in sum values

toSnafu :: Int -> String
toSnafu n =
  let decimals = toSnafuDecimals n
   in map toSnafuDigit decimals

table :: [(Char, Int)]
table = [('=', -2), ('-', -1), ('0', 0), ('1', 1), ('2', 2)]

fromTable :: (Show a, Foldable t, Eq a) => t (a, b) -> a -> b
fromTable table d =
  let res = find ((== d) . fst) table
      err = error ("invalid SNAFU digit " ++ show d)
   in maybe err snd res

fromSnafuDigit :: Char -> Int
fromSnafuDigit = fromTable table

toSnafuDigit :: Int -> Char
toSnafuDigit = fromTable (map (\(a, b) -> (b, a)) table)

toSnafuDecimals :: Int -> [Int]
toSnafuDecimals n =
  case n of
    0 -> []
    1 -> [1]
    2 -> [2]
    3 -> [1, -2]
    4 -> [1, -1]
    _ ->
      let (q, r) = quotRem n 5
          q' = toSnafuDecimals q ++ [0]
          r' = toSnafuDecimals r
       in snafuAdd q' r'

snafuAdd :: [Int] -> [Int] -> [Int]
snafuAdd a b =
  let res = reverse (snafuAddRec (reverse a) (reverse b) 0)
   in dropWhile (== 0) res
  where
    snafuAddRec :: [Int] -> [Int] -> Int -> [Int]
    snafuAddRec a b c =
      case (a, b, c) of
        ([], [], c) -> [c]
        ([], b, c) -> snafuAddRec [c] b 0
        (a, [], c) -> snafuAddRec a [c] 0
        (a : as, b : bs, c) ->
          let s = a + b + c
           in case s of
                s | s <= -3 -> 5 + s : snafuAddRec as bs (-1)
                s | -3 < s && s < 3 -> s : snafuAddRec as bs 0
                s | s >= 3 -> s - 5 : snafuAddRec as bs 1
                _ -> error ("snafuAddInt: invalid subtotal" ++ show s)

-- Debug output ---------------------------------------------------------------

rightAlign :: Int -> String -> String
rightAlign width str =
  let s = str
      padding = replicate (width - length s) ' '
   in padding ++ s

putTable :: [(String, Int)] -> Int -> IO ()
putTable pairs total = do
  let width = maximum (map (length . fst) pairs)
  let align = rightAlign width
  let formatted = map (\(s, d) -> align s ++ "  " ++ align (show d)) pairs

  putStrLn (align "SNAFU" ++ "  " ++ align "Decimal")
  mapM_ putStrLn formatted
  putStrLn ""
  putStrLn (rightAlign (2 * width + 2) ("Total: " ++ show total))

-- "Tests" :-) ----------------------------------------------------------------

assertPart1 :: Bool -> String -> String
assertPart1 isSample result =
  let expected =
        if isSample
          then "2=-1=0"
          else "2-2--02=1---1200=0-1"
   in if result == expected
        then result
        else
          error
            "Part 1 failed (expected \""
            ++ expected
            ++ "\", got \""
            ++ result
            ++ "\")."

-- Main -----------------------------------------------------------------------

main :: IO ()
main = do
  fileName <- head <$> getArgs
  snafuNumbers <- lines <$> readFile fileName
  let isSample = fileName /= "input.txt"

  let results = map fromSnafu snafuNumbers
  let total = sum results
  let part1 = assertPart1 isSample (toSnafu total)

  when isSample $ do putTable (zip snafuNumbers results) total
  putStrLn ""
  putStrLn ("Part 1: " ++ part1)
