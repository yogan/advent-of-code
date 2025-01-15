# [Advent of Code 2024](https://adventofcode.com/2024) (50/50 ✨)

**Languages: 12** *(Awk, Bash, C#, Dart, Elixir, Fortran, Gleam, Haskell, Idris,
Julia, 24 × Python, Vim)*

## [Day 01: Historian Hysteria](https://adventofcode.com/2024/day/1) 📜
  - ⭐⭐ [**A**wk](day-01-awk/aoc.awk)
  - ⭐⭐ [Vim](../vim/2024/day-01/aoc.vim)

## [Day 02: Red-Nosed Reports](https://adventofcode.com/2024/day/2) 🔴
  - ⭐⭐ [**B**ash](day-02-bash/aoc.bash)
  - ⭐⭐ [Python](day-02-python/aoc.py)

## [Day 03: Mull It Over](https://adventofcode.com/2024/day/3) ✖️
  - ⭐⭐ [**C**#](day-03-csharp/AoC.cs) ([tests](day-03-csharp/AoCTests.cs))
  - ⭐⭐ [Python](day-03-python/aoc.py)

## [Day 04: Ceres Search](https://adventofcode.com/2024/day/4) 🔠
  - ⭐⭐ [**D**art](day-04-dart/lib/aoc.dart) 
    ([tests](day-04-dart/test/aoc_test.dart))
  - ⭐⭐ [Python](day-04-python/aoc.py)

## [Day 05: Print Queue](https://adventofcode.com/2024/day/5) 🖨️
  - ⭐⭐ [**E**lixir](day-05-elixir/lib/aoc.ex) 
    ([tests](day-05-elixir/test/aoc_test.exs),
    [main](day-05-elixir/lib/main.ex))
  - ⭐⭐ [Python](day-05-python/aoc.py)

## [Day 06: Guard Gallivant](https://adventofcode.com/2024/day/6) 🛡️
  - ⭐⭐ [**F**ortran](day-06-fortran/aoc.f90)
    ([tests](day-06-fortran/tests.f90), [main](day-06-fortran/main.f90);
    reminder to myself: never use Fortran again or I'll go insane)
  - ⭐⭐ [Python](day-06-python/aoc.py)

## [Day 07: Bridge Repair](https://adventofcode.com/2024/day/7) 🌉
  - ⭐⭐ [**G**leam](day-07-gleam/src/aoc.gleam)
    ([tests](day-07-gleam/test/aoc_test.gleam))
  - ⭐⭐ [Python](day-07-python/aoc.py) using
    [`itertools.product`](https://docs.python.org/3/library/itertools.html#itertools.product)
    and brute force (run time is ~1 second, so it's fine)

## [Day 08: Resonant Collinearity](https://adventofcode.com/2024/day/8) 📡
  - ⭐⭐ [**H**askell](day-08-haskell/src/Lib.hs)
    ([tests](day-08-haskell/test/Spec.hs), [main](day-08-haskell/app/Main.hs))
  - ⭐⭐ [Python](day-08-python/aoc.py)

## [Day 09: Disk Fragmenter](https://adventofcode.com/2024/day/9) 💾
  - ⭐⭐ [**I**dris 2](day-09-idris2/AoC.idr)
    ([tests](day-09-idris2/Tests.idr), [main](day-09-idris2/Main.idr))
  - ⭐⭐ [Python](day-09-python/aoc.py) quite primitive solution with an array
    and index pointers; run time ~7 seconds

## [Day 10: Hoof It](https://adventofcode.com/2024/day/10) 🌋
  - ⭐⭐ [**J**ulia](day-10-julia/src/AoC.jl)
    ([tests](day-10-julia/test/runtests.jl), [main](day-10-julia/src/main.jl))
  - ⭐⭐ [Python](day-10-python/aoc.py)

## [Day 11: Plutonian Pebbles](https://adventofcode.com/2024/day/11) 🪨
  - ⭐⭐ [Python](day-11-python/aoc.py)

## [Day 12: Garden Groups](https://adventofcode.com/2024/day/12) 🪴
  - ⭐⭐ [Python](day-12-python/aoc.py) pretty tough part 2

## [Day 13: Claw Contraption](https://adventofcode.com/2024/day/13) 🏗️
  - ⭐⭐ [Python](day-13-python/aoc.py) zomg math

## [Day 14: Restroom Redoubt](https://adventofcode.com/2024/day/14) 🚽
  - ⭐⭐ [Python](day-14-python/aoc.py) part 1 is simple modulo arithmetic, part
    2 is wild - finding the Christmas tree was not easy, I ended up with a quite
    slow (~ 40 sec) solution that checks for the existence of a long horizontal
    line of robots, which indicates something visually interesting

## [Day 15: Warehouse Woes](https://adventofcode.com/2024/day/15) 📦
  - ⭐⭐ [Python](day-15-python/aoc.py)

## [Day 16: Reindeer Maze](https://adventofcode.com/2024/day/16) 🦌
  - ⭐⭐ [Python](day-16-python/aoc.py) Dijkstra with priority queue (part 1),
    BFS for nodes on all shortest paths (part 2)

## [Day 17: Chronospatial Computer](https://adventofcode.com/2024/day/17) 🖥️
  - ⭐⭐ [Python](day-17-python/aoc.py) Part 1 is just simulating a CPU with
    some op codes, but part 2 was really wild. You have to find a register
    starting value that leads to the program outputting itself, turning it into
    a [quine](https://en.wikipedia.org/wiki/Quine_(computing)). I had to take
    some hints to point me in the right direction here. Analyzing the decompiled
    program shows that the register is used in a loop to produce the output, and
    that only the lowest 3 bits of the registered are used per iteration. The
    register value is then shifted right by 3 bits. This allows to find the
    right starting value by trying bit triplets for each of the output digits.

## [Day 18: RAM Run](https://adventofcode.com/2024/day/18) 🏃
  - ⭐⭐ [Python](day-18-python/aoc.py) Refreshingly easy day after the horrors
    of day 17 part 2. Part 1 is a simple shortest path search, part 2 is just
    checking when there is no more path, which can actually be brute forced in a
    very naive way, or sped up significantly by using a binary search.

## [Day 19: Line Layout](https://adventofcode.com/2024/day/19) 🧵
  - ⭐⭐ [Python](day-19-python/aoc.py) recursion goes brrr

## [Day 20: Race Condition](https://adventofcode.com/2024/day/20) 🏁
  - ⭐⭐ [Python](day-20-python/aoc.py) Very cool problem! Took some time to get
    to a part 2 solution that is fast enough, but the end result is fantastic
    and also solves part 2 easily.

## [Day 21: Keypad Conundrum](https://adventofcode.com/2024/day/21) 🔢
  - ⭐⭐ [Python](day-21-python/aoc.py) dpad inception == pure horror

## [Day 22: Monkey Market](https://adventofcode.com/2024/day/22) 🍌
  - ⭐⭐ [Python](day-22-python/aoc.py) quite relaxing and nice after the day 21
    insanity; part 2: sliding window with extra steps

## [Day 23: LAN Party](https://adventofcode.com/2024/day/23) 🕹️
  - ⭐⭐ [Python](day-23-python/aoc.py) finding (maximum) cliques in a graph;
  initially got a super slow (~ 1 h) solution for part 2, then switched to the
  [Bron-Kerbosch
  algorithm](https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm),
  which is really fast for the given graph

## [Day 24: Crossed Wires](https://adventofcode.com/2024/day/24) 🔀
  - ⭐⭐ [Python](day-24-python/aoc.py) wow, part 2 is one mess of a brute force
    solution, but it works (run time ~ 30 sec)

## [Day 25: Code Chronicle](https://adventofcode.com/2024/day/25) 🔑
  - ⭐⭐ [Python](day-25-python/aoc.py)
