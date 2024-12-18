# [Advent of Code 2024](https://adventofcode.com/2024) (34/50 ✨)

**Languages: 7** *(1 × Awk, 1 × Bash, 1 × C#, 1 × Dart, 1 × Elixir, ½ ×
Fortran, 15 × Python, 1 × Vim)*

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
  - ⭐ [**F**ortran](day-06-fortran/aoc.f90)
    ([tests](day-06-fortran/tests.f90), [main](day-06-fortran/main.f90),
    part 1 only)
  - ⭐⭐ [Python](day-06-python/aoc.py)

## [Day 07: Bridge Repair](https://adventofcode.com/2024/day/7) 🌉
  - ⭐⭐ [Python](day-07-python/aoc.py) using
    [`itertools.product`](https://docs.python.org/3/library/itertools.html#itertools.product)
    and brute force (run time is ~1 second, so it's fine)

## [Day 08: Resonant Collinearity](https://adventofcode.com/2024/day/8) 📡
  - ⭐⭐ [Python](day-08-python/aoc.py)

## [Day 09: Disk Fragmenter](https://adventofcode.com/2024/day/9) 💾
  - ⭐⭐ [Python](day-09-python/aoc.py) quite primitive solution with an array
    and index pointers; run time ~7 seconds

## [Day 10: Hoof It](https://adventofcode.com/2024/day/10) 🌋
  - ⭐⭐ [Python](day-10-python/aoc.py)

## [Day 11: Plutonian Pebbles](https://adventofcode.com/2024/day/11) 🪨
  - ⭐⭐ [Python](day-11-python/aoc.py)

## [Day 12: Garden Groups](https://adventofcode.com/2024/day/12) 🪴
  - ⭐⭐ [Python](day-12-python/aoc.py) pretty tough part 2

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
