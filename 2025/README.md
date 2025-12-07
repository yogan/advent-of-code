# [Advent of Code 2025](https://adventofcode.com/2025) (14/25 ✨)

## [Day 01: Secret Entrance](https://adventofcode.com/2025/day/1) 🚪
  - ⭐⭐ [Python](day-01-python/aoc.py)
  - ⭐⭐ [Gleam](day-01-gleam/src/aoc.gleam) ([tests](day-01-gleam/test/aoc_test.gleam))

## [Day 02: Gift Shop](https://adventofcode.com/2025/day/2) 🎁
  - ⭐⭐ [Python](day-02-python/aoc.py)
  - ⭐⭐ [Rust](day-02-rust/src/main.rs)

## [Day 03: Lobby](https://adventofcode.com/2025/day/3) 🔋
  - ⭐⭐ [Python](day-03-python/aoc.py)
  - ⭐⭐ [Haskell](day-03-haskell/src/Lib.hs) ([main](day-03-haskell/app/Main.hs), [tests](day-03-haskell/test/Spec.hs))
  - yay, recursion

## [Day 04: Printing Department](https://adventofcode.com/2025/day/4) 🧻
  - ⭐⭐ [Python](day-04-python/aoc.py)
    - optimized from initially 167 ms to 60 ms by introducing a dirty set of
      positions to check each round
  - ⭐⭐ [Zig](day-04-zig/aoc.zig)
    - no dirty set, still sort of fast (75 ms)

## [Day 05: Cafeteria](https://adventofcode.com/2025/day/5) ☕
  - ⭐⭐ [Python](day-05-python/aoc.py)
    - interval merging (explicit, with sets)
  - ⭐⭐ [Elixir](day-05-elixir/lib/aoc.ex) ([main](day-05-elixir/lib/main.ex), [tests](day-05-elixir/test/aoc_test.exs))
    - sorting ranges by lower boundary (implicit merging)

## [Day 06: Trash Compactor](https://adventofcode.com/2025/day/6) 🗑️
  - ⭐⭐ [Python](day-06-python/aoc.py)
    - slapping that good old `zip(*lines[::-1])` against the problem
  - ⭐⭐ [Julia](day-06-julia/src/AoC.jl) ([main](day-06-julia/src/main.jl), [tests](day-06-julia/test/runtests.jl))
    - hooray, Julia has `rotr90` and `stack`

## [Day 07: Laboratories](https://adventofcode.com/2025/day/7) 🔬
  - ⭐⭐ [Python](day-07-python/aoc.py)
  - cool problem, counting multiverses is fun
