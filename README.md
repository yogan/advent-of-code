# [Advent of Code](https://adventofcode.com)

[![Docker image](https://github.com/yogan/advent-of-code/actions/workflows/build-docker-image.yml/badge.svg)](https://github.com/yogan/advent-of-code/actions/workflows/build.yml)
[![Solutions](https://github.com/yogan/advent-of-code/actions/workflows/test-solutions.yml/badge.svg)](https://github.com/yogan/advent-of-code/actions/workflows/test-solutions.yml)
[![Templates](https://github.com/yogan/advent-of-code/actions/workflows/test-templates.yml/badge.svg)](https://github.com/yogan/advent-of-code/actions/workflows/test-templates.yml)

## [2023](https://adventofcode.com/2023) (44/50 ✨)

**Languages: 5** *(19 × Python, 1 × fish, 1 × Clojure, 1 × DDP, 1 × Zig)*

- [Day 01](https://adventofcode.com/2023/day/1) ⭐⭐ in
  [fish](2023/day-01/day01.fish)
  with [tests](2023/day-01/test.fish) 🐟
- [Day 02](https://adventofcode.com/2023/day/2) ⭐⭐ in
  [Clojure](2023/day-02/src/advent_of_code_template/core.clj)
  with [tests](2023/day-02/test/advent_of_code_template/core_test.clj) 📃
- [Day 03](https://adventofcode.com/2023/day/3) ⭐⭐ in
  [Python](2023/day-03/day03.py) 🐍
- [Day 04](https://adventofcode.com/2023/day/4) ⭐⭐ in
  [DDP - Die Deutsche Programmiersprache](2023/day-04/Tag4.ddp) 🥨
- [Day 05](https://adventofcode.com/2023/day/5) ⭐⭐ in
  [Python](2023/day-05/day05.py)
  - throwing unit tests against functions until stuff works out…
  - efficient, but complicated range based solution for *part 2* 📏
- [Day 06](https://adventofcode.com/2023/day/6) ⭐⭐ in
  [Zig](2023/day-06/src/main.zig)
  - including a memory leak that I could not find
  - Zig is really hard 😢
- [Day 07](https://adventofcode.com/2023/day/7) ⭐⭐ in
  [Python](2023/day-07/day07.py)
  - half smart, half brute force is the real Joker 🃏
- [Day 08](https://adventofcode.com/2023/day/8) ⭐⭐ in
  [Python](2023/day-08/day08.py)
  - haunted solution, LCM works for some reason 👻
- [Day 09](https://adventofcode.com/2023/day/9) ⭐⭐ in
  [Python](2023/day-09/day09.py)
  - easy and straightforward 🏝️
- [Day 10](https://adventofcode.com/2023/day/10) ⭐⭐ in
  [Python](2023/day-10/day10.py)
  - pretty lengthy, but it prints some nice
    [Unicode visualization](2023/day-10/README.md) ꡌ
- [Day 11](https://adventofcode.com/2023/day/11) ⭐⭐ in
  [Python](2023/day-11/day11.py)
  - space math 🌌
- [Day 12](https://adventofcode.com/2023/day/12) ⭐⭐ in
  [Python](2023/day-12/day12.py) 🤯
  - *part 1:* initially brute force generating valid patterns (with some
    optimizations)
  - *part 2:* complete rewrite: recursive count of valid patterns with
    memoization (took some inspiration for this…)
 - [Day 13](https://adventofcode.com/2023/day/13) ⭐⭐ in
   [Python](2023/day-13/day13.py) 🪞
   - *part 1:* just iterating over 2D arrays and comparing strings
   - *part 2:* brute forcing over the patterns with one entry swapped at each
     position until a new row or column is found
   - notable Python tricks:
     - `list(zip(*arr))` transposes an array, so that columns can be treated as rows
     - a [`for` loop can have an `else` block](https://docs.python.org/3/tutorial/controlflow.html#break-and-continue-statements-and-else-clauses-on-loops) – this can be used to `break` an outer loop
 - [Day 14](https://adventofcode.com/2023/day/14) ⭐⭐ in
   [Python](2023/day-14/day14.py) 📡
   - *part 1:* moving stuff around in arrays (rotating a 2D array helps so
     that only one direction has to be implemented - shifting east is easiest,
     as we can go line by line and within a line from left to right)
   - *part 2:* finding cycles and not messing up modulo calculations
 - [Day 15](https://adventofcode.com/2023/day/15) ⭐⭐ in
   [Python](2023/day-15/day15.py) 🔍
   - straightforward coding, one of the easiest days so far
 - [Day 16](https://adventofcode.com/2023/day/16) ⭐⭐ in
   [Python](2023/day-16/day16.py) 🌋
   - *part 1:* BFS (queue work list + visited set)
   - *part 2:* brute-force of *part 1* with all starting positions (not that
     many, run-time is around 1.5 sec)
   - [terminal visualization using curses](2023/day-16/README.md)
     - `char.translate(char.maketrans("RLUD", "→←↑↓")` is a neat trick
 - [Day 17](https://adventofcode.com/2023/day/17) ⭐⭐ in
   [Python](2023/day-17/day17.py) 🫕
   - *part 1:* Dijkstra with priority queue (`heapq`); the tricky part is to
     include both direction and steps already taken in that direction into the
     queue and seen set
   - *part 2:* making max steps configurable and adding a min steps in same
     direction was easy, but everything broke because I started with a single
     entry in the queue with a fake direction of `(0, 0)`, which messed up the
     minimum step count; solved by adding the start twice, with right and down
     directions (`(0, 1)` and `(1, 0)`)
 - [Day 18](https://adventofcode.com/2023/day/18) ⭐⭐ in
   [Python](2023/day-18/day18.py) ⛏️
   - *part 1:* initially solved with a flood fill, but…
   - *part 2:* … is way to big for a flood fill, so I had to look up some math:
     - the [shoelace formula](https://en.wikipedia.org/wiki/Shoelace_formula) is
       a simple and fast way to calculate the area of a polygon (Mathologer
       has a [very nice video](https://www.youtube.com/watch?v=0KjG8Pg6LGk)
       about this)
     - with the area and the number of border points (from part 1), we can
       derive the number of inner points via
       [Pick's theorem](https://en.wikipedia.org/wiki/Pick%27s_theorem)
 - [Day 19: Aplenty](https://adventofcode.com/2023/day/19) ⭐⭐ in
   [Python](2023/day-19/day19.py) 🔧
   - *part 1* went pretty well; putting the input into some proper data
     structures and then iterating over the parts and traversing the workflow
     graph with each of them
   - *part 2* was brutal - hardest day for me so far; since it took me a while,
     I added a [write-up of my final algorithm](2023/day-19/README.md)
 - [Day 20: Pulse Propagation](https://adventofcode.com/2023/day/20) ⭐⭐ in
   [Python](2023/day-20/day20.py) 🔀
   - *part 1:* implementing the modules and their behavior/states, then
     simulating the whole thing
   - *part 2:* the solution is based on an observation about the structure of
     the machine; with this known, cycle lengths of sub-machines can be found,
     and the final result is the LCM of all cycle lengths; see the lengthy
     comments of
     [`find_circle_outputs()`](https://github.com/yogan/advent-of-code/blob/main/2023/day-20/day20.py#L141) and 
     [`part2()`](https://github.com/yogan/advent-of-code/blob/main/2023/day-20/day20.py#L187) for details
 - [Day 21: Step Counter](https://adventofcode.com/2023/day/21) ⭐✖️ in
   [Python](2023/day-21/day21.py) 👣
   - *part 1:* BFS on grid
   - *part 2:* missing…
 - [Day 22: Sand Slabs](https://adventofcode.com/2023/day/22) ⭐⭐ in
   [Python](2023/day-22/day22.py) 🧱
   - initially I tried a clever way to determine if a brick can be
     disintegrated, but there is some bug that I cannot find - the function
     [`can_be_disintegrated()`](https://github.com/yogan/advent-of-code/blob/main/2023/day-22/day22.py#L74)
     (still commented out in the code) detects more bricks as disintegratable
     than it should
   - after a lot of debugging, I gave up and re-used the
     [`drop()`](https://github.com/yogan/advent-of-code/blob/main/2023/day-22/day22.py#L48)
     function; calling that on sets of bricks with one brick removed and
     checking the bricks that fell takes a good amount of time, but gives the
     data that is needed for both *part 1* and *part 2*
   - the excessive debugging at least produced some nice 3D plots created with
     [Matplotlib](https://matplotlib.org) (see [day 22 README](2023/day-22/README.md))
 - [Day 23: A Long Walk](https://adventofcode.com/2023/day/23) ⭐✖️ in
   [Python](2023/day-23/day23.py) 🚶
   - *part 1:* DFS; to get the different paths, the partial paths are stored in
     the work queue, and used to re-initialize the visited set after a complete
     path has been found
   - *part 2:* missing…
   - built some nice [terminal visualization](2023/day-23/README.md)
 
## [2022](https://adventofcode.com/2022) (46/50 ✨)

**Languages: 13** *(fish, JavaScript, TypeScript, Perl, Ruby, C, Java, Fortran, Nim,
Lua, C#, Python, Haskell)*  
**Ideas for 2023:** see [2022/languages.md](2022/languages.md)

- [Day 01](https://adventofcode.com/2022/day/1) ⭐⭐ in
  [fish](2022/day-01/day01.fish)
  with [tests](2022/day-01/test.fish)
- [Day 02](https://adventofcode.com/2022/day/2) ⭐⭐ in
  [JavaScript (Node/esm)](2022/day-02/day02.mjs),
  [tests](2022/day-02/day02.test.mjs) for Node 19's experimental built-in test runner
- [Day 03](https://adventofcode.com/2022/day/3) ⭐⭐ in
  [TypeScript (Bun)](2022/day-03/day03.ts),
  [tests](2022/day-03/day03.test.ts) require Bun Canary, see [README](2022/day-03/README.md)
- [Day 04](https://adventofcode.com/2022/day/4) ⭐⭐ in
  [Perl](2022/day-04/day04.pl) with [Test2](https://metacpan.org/pod/Test2)
- [Day 05](https://adventofcode.com/2022/day/5) ⭐⭐ in
  [Ruby](2022/day-05/day05.rb)
- [Day 06](https://adventofcode.com/2022/day/6) ⭐⭐ in
  [C](2022/day-06/day06.c) 💀
- [Day 07](https://adventofcode.com/2022/day/7) ⭐⭐ in
  [Java](2022/day-07/src/main/java/de/zogan/aoc2022/Day07.java)
  ([tests](2022/day-07/src/test/java/de/zogan/aoc2022/Day07Tests.java)) 🦕
- [Day 08](https://adventofcode.com/2022/day/8) ⭐⭐ in
  [Fortran](2022/day-08/day08.f90)
  ([tests](2022/day-08/tests.f90)) 🧑‍🔬 with GOTO!
  (paired with [@raborlattinchen](https://github.com/raborlattinchen))
- [Day 09](https://adventofcode.com/2022/day/9) ⭐⭐ in
  [Nim](2022/day-09/day09.nim) visualizing *does* help… I learned the hard way
- [Day 10](https://adventofcode.com/2022/day/10) ⭐⭐ in
  [Lua](2022/day-10/day10.lua) ([tests](2022/day-10/day10_spec.lua))
- [Day 11](https://adventofcode.com/2022/day/11) ⭐⭐ in
  [C#](2022/day-11/Day11.cs) ([tests](2022/day-11/Day11Tests.cs),
  [main](2022/day-11/Program.cs)) with record structs
- [Day 12](https://adventofcode.com/2022/day/12) ⭐⭐ in
  [Python](2022/day-12/day12.py) no tests, but quickly done
- [Day 13](https://adventofcode.com/2022/day/13) ⭐⭐ in
  [Python](2022/day-13/day13.py) input data is recursive and almost valid Python syntax, so Python /w `eval`
- [Day 14](https://adventofcode.com/2022/day/14) ⭐⭐ in
  [Python](2022/day-14/day14.py) with [visualization](2022/day-14/README.md)
- [Day 15](https://adventofcode.com/2022/day/15) ⭐⭐ in
  [Python](2022/day-15/day15.py) "Use the force, Brute!"  
  Learned about *PyPy* and *tqdm* (see [day 15 README](2022/day-15/README.md)),
  both very helpful for long-running Python scripts
- [Day 16](https://adventofcode.com/2022/day/16) ️✖️✖️
- [Day 17](https://adventofcode.com/2022/day/17) ⭐⭐ in
  [Python](2022/day-17/day17.py) used some hints for the cycle detection of part 2
- [Day 18](https://adventofcode.com/2022/day/18) ⭐⭐ in
  [Python](2022/day-18/day18.py) with [3D plots](2022/day-18/README.md)
- [Day 19](https://adventofcode.com/2022/day/19) ⭐⭐ in
  [Python](2022/day-19/day19.py) absolutely crazy, a lot of tweaking of guessed
  parameters to throw away states so that everything fits into memory
- [Day 20](https://adventofcode.com/2022/day/20) ⭐⭐ in
  [Python](2022/day-20/day20.py) modulo goes brrrr
- [Day 21](https://adventofcode.com/2022/day/21) ⭐⭐ in
  [Python](2022/day-21/day21.py) expression trees and brute force binary search (with a bit of luck)
- [Day 22](https://adventofcode.com/2022/day/22) ⭐✖️ in
  [Python](2022/day-22/day22.py) part 1 only
- [Day 23](https://adventofcode.com/2022/day/23) ⭐⭐ in
  [Python](2022/day-23/day23.py) actually quite nice and clean code (pattern matching, list/set comprehensions)
- [Day 24](https://adventofcode.com/2022/day/24) ⭐⭐ in
  [Python](2022/day-24/day24.py) BFS (caching blizzard cycles == *huge* performance boost)
- [Day 25](https://adventofcode.com/2022/day/25) ⭐✖️ in
  [Haskell](2022/day-25/day25.hs) 𝝺 last day, took the chance to increase my language count

## [2021](https://adventofcode.com/2021) (34/50 ✨)

- [Day 01](https://adventofcode.com/2021/day/1) ⭐⭐ in [Python](2021-python/day01.py)
- [Day 02](https://adventofcode.com/2021/day/2) ⭐⭐ in [Python](2021-python/day02.py) (paired with [LaLisita](https://github.com/LaLisita))
- [Day 03](https://adventofcode.com/2021/day/3) ⭐⭐ in [Python](2021-python/day03.py) (paired with [LaLisita](https://github.com/LaLisita))
- [Day 04](https://adventofcode.com/2021/day/4) ⭐⭐ in [Python](2021-python/day04.py) (paired with [LaLisita](https://github.com/LaLisita))
- [Day 05](https://adventofcode.com/2021/day/5) ⭐⭐ in [Python](2021-python/day05.py)
- [Day 06](https://adventofcode.com/2021/day/6) ⭐⭐ in [Python](2021-python/day06.py) (paired with [LaLisita](https://github.com/LaLisita))
- [Day 07](https://adventofcode.com/2021/day/7) ⭐⭐ in [Python](2021-python/day07.py)
- [Day 08](https://adventofcode.com/2021/day/8) ⭐ in [Python](2021-python/day08.py) (paired with [LaLisita](https://github.com/LaLisita))
- [Day 09](https://adventofcode.com/2021/day/9) ⭐⭐ in [Python](2021-python/day09.py) (paired with [LaLisita](https://github.com/LaLisita))
- [Day 10](https://adventofcode.com/2021/day/10) ⭐⭐ in [Python](2021-python/day10.py) (paired with [dnnr](https://github.com/dnnr))
- [Day 11](https://adventofcode.com/2021/day/11) ⭐⭐ in [Rust (*external repo*)](https://github.com/dnnr/advent-of-code-2021/blob/master/src/day11/mod.rs) (paired with [dnnr](https://github.com/dnnr))
- [Day 12](https://adventofcode.com/2021/day/12) ⭐⭐ in [Python](2021-python/day12.py)
- [Day 13](https://adventofcode.com/2021/day/13) ⭐⭐ in [Python](2021-python/day13.py) ASCII art <3
- [Day 14](https://adventofcode.com/2021/day/14) ⭐⭐ in [Python](2021-python/day14.py) "I can count to potato!"
- [Day 15](https://adventofcode.com/2021/day/15) ⭐⭐ in [Python](2021-python/day15.py) initially slow Dijkstra, boosted /w priority queue for part 2
- [Day 20](https://adventofcode.com/2021/day/20) ⭐ in [Python](2021-python/day20.py)
- [Day 21](https://adventofcode.com/2021/day/21) ⭐⭐ in [Python](2021-python/day21.py) powered by galaxy brain
- [Day 22](https://adventofcode.com/2021/day/22) ⭐ in [Python](2021-python/day22.py)
- [Day 25](https://adventofcode.com/2021/day/25) ⭐⭐ in [Python](2021-python/day25.py)

## [2020](https://adventofcode.com/2020) (30/50 ✨)

- [Day 01](https://adventofcode.com/2020/day/1) ⭐⭐ in [Rust](2020-rust/src/day01.rs) (paired with [t-animal](https://github.com/t-animal) and [RussellSnyder](https://github.com/RussellSnyder))
- [Day 02](https://adventofcode.com/2020/day/2) ⭐⭐ in [Rust](2020-rust/src/day02.rs) (paired with [dnnr](https://github.com/dnnr) and [RussellSnyder](https://github.com/RussellSnyder))
- [Day 03](https://adventofcode.com/2020/day/3) ⭐⭐ in [Rust](2020-rust/src/day03.rs) (paired with [dnnr](https://github.com/dnnr) and [RussellSnyder](https://github.com/RussellSnyder))
- [Day 04](https://adventofcode.com/2020/day/4) ⭐⭐ in [Rust](2020-rust/src/day04.rs) (paired with [dnnr](https://github.com/dnnr) and [RussellSnyder](https://github.com/RussellSnyder))
- [Day 05](https://adventofcode.com/2020/day/5) ⭐⭐ in [Rust](2020-rust/src/day05.rs) (paired with [dnnr](https://github.com/dnnr)) "oh look we found how unit tests work"
- [Day 06](https://adventofcode.com/2020/day/6) ⭐⭐ in [Rust](2020-rust/src/day06.rs) (paired with [dnnr](https://github.com/dnnr))
- [Day 07](https://adventofcode.com/2020/day/7) ⭐⭐ in [Rust](2020-rust/src/day07.rs) "to understand recursion, first you have to understand recursion"
- [Day 08](https://adventofcode.com/2020/day/8) ⭐⭐ in [Rust](2020-rust/src/day08.rs) (paired with [dnnr](https://github.com/dnnr)) Rust can solve the halting problem 🤯
- [Day 09](https://adventofcode.com/2020/day/9) ⭐⭐ in [Rust](2020-rust/src/day09.rs) (paired with [dnnr](https://github.com/dnnr) and [RussellSnyder](https://github.com/RussellSnyder))
- [Day 10](https://adventofcode.com/2020/day/10) ⭐⭐ in [Rust](2020-rust/src/day10.rs) Mappy McMapface
- [Day 12](https://adventofcode.com/2020/day/12) ⭐⭐ in [Rust](2020-rust/src/day12.rs) (paired with [dnnr](https://github.com/dnnr))
- [Day 13](https://adventofcode.com/2020/day/13) ⭐⭐ in [Rust](2020-rust/src/day13.rs) (paired with [dnnr](https://github.com/dnnr)) part 2 via Wolfram Alpha 🥺👉👈
- [Day 14](https://adventofcode.com/2020/day/14) ⭐⭐ in [Rust](2020-rust/src/day14.rs) (paired with [dnnr](https://github.com/dnnr)) with some OO and enum, but ugly mask code
- [Day 15](https://adventofcode.com/2020/day/15) ⭐⭐ in [Rust](2020-rust/src/day15.rs) (paired with [RussellSnyder](https://github.com/RussellSnyder) and [dnnr](https://github.com/dnnr)) "we test in production" edition
- [Day 16](https://adventofcode.com/2020/day/16) ⭐⭐ in [Rust](2020-rust/src/day16.rs) (paired with [RussellSnyder](https://github.com/RussellSnyder)) loop da whoop

## [2015](https://adventofcode.com/2015) (4/50 ✨)

- [Day 01](https://adventofcode.com/2015/day/1) ⭐⭐ in
  [Clojure](2015/day-01/src/advent_of_code_template/core.clj) ([Tests](2015/day-01/test/advent_of_code_template/core_test.clj))
- [Day 02](https://adventofcode.com/2015/day/2) ⭐⭐ in
  [DDP (Die Deutsche Programmiersprache)](2015/day-02/2015-Tag-02.ddp)
