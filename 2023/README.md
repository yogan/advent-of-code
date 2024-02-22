# [Advent of Code 2023](https://adventofcode.com/2023) (50/50 ✨)

**Languages: 6** *(21 × Python, 1 × fish, 1 × Clojure, 1 × DDP, 1 × Zig, 4 × Vim)*

## [Day 01: Trebuchet?!](https://adventofcode.com/2023/day/1) 🚀
  - ⭐⭐ [fish](day-01-fish/day01.fish) with [tests](day-01-fish/test.fish) 🐟
  - ⭐⭐ [Vim](../vim/2023/day-01/aoc-2023-01.vim)
    [(commented)](../vim/2023/day-01/aoc-2023-01.commented.vim)

## [Day 02: Cube Conundrum](https://adventofcode.com/2023/day/2) 🧊
  - ⭐⭐ [Clojure](day-02-clojure/src/advent_of_code_template/core.clj)
    with [tests](day-02-clojure/test/advent_of_code_template/core_test.clj) 📃
  - ⭐⭐ [Vim](../vim/2023/day-02/aoc-2023-02.vim)
    [(commented)](../vim/2023/day-02/aoc-2023-02.commented.vim)

## [Day 03: Gear Ratios](https://adventofcode.com/2023/day/3) ⚙️
  - ⭐⭐ [Python](day-03-python/day03.py) 🐍
  - ⭐⭐ [Vim](../vim/2023/day-03/aoc-2023-03.vim)
    [(commented)](../vim/2023/day-03/aoc-2023-03.commented.vim)

## [Day 04: Scratchcards](https://adventofcode.com/2023/day/4)
  - ⭐⭐ [DDP - Die Deutsche Programmiersprache](day-04-ddp/Tag4.ddp) 🥨
  - ⭐⭐ [Vim](../vim/2023/day-04/aoc-2023-04.vim)
    [(commented)](../vim/2023/day-04/aoc-2023-04.commented.vim)

## [Day 05: If You Give A Seed A Fertilizer](https://adventofcode.com/2023/day/5) 🌱
  - ⭐⭐ [Python](day-05-python/day05.py)
  - throwing unit tests against functions until stuff works out…
  - efficient, but complicated range based solution for *part 2* 📏

## [Day 06: Wait For It](https://adventofcode.com/2023/day/6) 🚤
  - ⭐⭐ [Zig](day-06-zig/src/main.zig)
  - including a memory leak that I could not find
  - Zig is really hard 😢

## [Day 07: Camel Cards](https://adventofcode.com/2023/day/7) 🐫
  - ⭐⭐ [Python](day-07-python/day07.py)
  - half smart, half brute force is the real Joker 🃏

## [Day 08: Haunted Wasteland](https://adventofcode.com/2023/day/8) 👻
  - ⭐⭐ [Python](day-08-python/day08.py)
  - haunted solution, LCM works for some reason

## [Day 09: Mirage Maintenance](https://adventofcode.com/2023/day/9) 🏝️
  - ⭐⭐ [Python](day-09-python/day09.py)
  - easy and straightforward

## [Day 10: Pipe Maze](https://adventofcode.com/2023/day/10) ꡌ
  - ⭐⭐ [Python](day-10-python/day10.py)
  - pretty lengthy, but it prints some nice
    [Unicode visualization](day-10-python/README.md)

## [Day 11: Cosmic Expansion](https://adventofcode.com/2023/day/11) 🌌
  - ⭐⭐ [Python](day-11-python/day11.py)
  - space math

## [Day 12: Hot Springs](https://adventofcode.com/2023/day/12) ♨️
  - ⭐⭐ [Python](day-12-python/day12.py)
  - *part 1:* initially brute force generating valid patterns (with some
    optimizations)
  - *part 2:* complete rewrite: recursive count of valid patterns with
    memoization (took some inspiration for this…) 🤯

## [Day 13: Point of Incidence](https://adventofcode.com/2023/day/13) 🪞
  - ⭐⭐ [Python](day-13-python/day13.py)
  - *part 1:* just iterating over 2D arrays and comparing strings
  - *part 2:* brute forcing over the patterns with one entry swapped at each
    position until a new row or column is found
  - notable Python tricks:
    - `list(zip(*arr))` transposes an array, so that columns can be treated as rows
    - a [`for` loop can have an `else` block](https://docs.python.org/3/tutorial/controlflow.html#break-and-continue-statements-and-else-clauses-on-loops) – this can be used to `break` an outer loop

## [Day 14: Parabolic Reflector Dish](https://adventofcode.com/2023/day/14) 📡
  - ⭐⭐ [Python](day-14-python/day14.py)
  - *part 1:* moving stuff around in arrays (rotating a 2D array helps so
    that only one direction has to be implemented - shifting east is easiest,
    as we can go line by line and within a line from left to right)
  - *part 2:* finding cycles and not messing up modulo calculations

## [Day 15: Lens Library](https://adventofcode.com/2023/day/15) 🔍
  - ⭐⭐ [Python](day-15-python/day15.py)
  - straightforward coding, one of the easiest days so far

## [Day 16: The Floor Will Be Lava](https://adventofcode.com/2023/day/16) 🌋
  - ⭐⭐ [Python](day-16-python/day16.py)
  - *part 1:* BFS (queue work list + visited set)
  - *part 2:* brute-force of *part 1* with all starting positions (not that
    many, run-time is around 1.5 sec)
  - ⭐⭐ [terminal visualization using curses](day-16-python/README.md)
    - `char.translate(char.maketrans("RLUD", "→←↑↓")` is a neat trick

## [Day 17: Clumsy Crucible](https://adventofcode.com/2023/day/17) 🫕
  - ⭐⭐ [Python](day-17-python/day17.py)
  - *part 1:* Dijkstra with priority queue (`heapq`); the tricky part is to
    include both direction and steps already taken in that direction into the
    queue and seen set
  - *part 2:* making max steps configurable and adding a min steps in same
    direction was easy, but everything broke because I started with a single
    entry in the queue with a fake direction of `(0, 0)`, which messed up the
    minimum step count; solved by adding the start twice, with right and down
    directions (`(0, 1)` and `(1, 0)`)

## [Day 18: Lavaduct Lagoon](https://adventofcode.com/2023/day/18) ⛏️
  - ⭐⭐ [Python](day-18-python/day18.py)
  - *part 1:* initially solved with a flood fill, but…
  - *part 2:* … is way to big for a flood fill, so I had to look up some math:
    - the [shoelace formula](https://en.wikipedia.org/wiki/Shoelace_formula) is
      a simple and fast way to calculate the area of a polygon (Mathologer
      has a [very nice video](https://www.youtube.com/watch?v=0KjG8Pg6LGk)
      about this)
    - with the area and the number of border points (from part 1), we can
      derive the number of inner points via
      [Pick's theorem](https://en.wikipedia.org/wiki/Pick%27s_theorem)

## [Day 19: Aplenty](https://adventofcode.com/2023/day/19) 🔧
  - ⭐⭐ [Python](day-19-python/day19.py)
  - *part 1* went pretty well; putting the input into some proper data
    structures and then iterating over the parts and traversing the workflow
    graph with each of them
  - *part 2* was brutal - hardest day for me so far; since it took me a while,
    I added a [write-up of my final algorithm](day-19-python/README.md)

## [Day 20: Pulse Propagation](https://adventofcode.com/2023/day/20) 🔀
  - ⭐⭐ [Python](day-20-python/day20.py)
  - *part 1:* implementing the modules and their behavior/states, then
    simulating the whole thing
  - *part 2:* the solution is based on an observation about the structure of
    the machine; with this known, cycle lengths of sub-machines can be found,
    and the final result is the LCM of all cycle lengths; see the lengthy
    comments of
    [`find_circle_outputs()`](https://github.com/yogan/advent-of-code/blob/main/2023/day-20-python/day20.py#L141) and 
    [`part2()`](https://github.com/yogan/advent-of-code/blob/main/2023/day-20-python/day20.py#L187) for details

## [Day 21: Step Counter](https://adventofcode.com/2023/day/21) 👣
  - ⭐⭐ [Python](day-21-python/day21.py)
  - *part 1:* BFS on grid
  - *part 2:* crazy calculations based on a diamond shape, took a very long
    time to get right; source has some lengthy comments including a lot of
    ASCII art drawings of the shapes

## [Day 22: Sand Slabs](https://adventofcode.com/2023/day/22) 🧱
  - ⭐⭐ [Python](day-22-python/day22.py)
  - initially I tried a clever way to determine if a brick can be
    disintegrated, but there is some bug that I cannot find - the function
    [`can_be_disintegrated()`](https://github.com/yogan/advent-of-code/blob/main/2023/day-22-python/day22.py#L74)
    (still commented out in the code) detects more bricks as disintegratable
    than it should
  - after a lot of debugging, I gave up and re-used the
    [`drop()`](https://github.com/yogan/advent-of-code/blob/main/2023/day-22-python/day22.py#L48)
    function; calling that on sets of bricks with one brick removed and
    checking the bricks that fell takes a good amount of time, but gives the
    data that is needed for both *part 1* and *part 2*
  - the excessive debugging at least produced some nice 3D plots created with
    [Matplotlib](https://matplotlib.org) (see [day 22 README](day-22-python/README.md))

## [Day 23: A Long Walk](https://adventofcode.com/2023/day/23) 🚶
  - ⭐⭐ [Python](day-23-python/day23.py)
  - *part 1:* DFS; to get the different paths, the partial paths are stored
    the work queue, and used to re-initialize the visited set after a complete
    path has been found
    - built some nice [terminal visualization](day-23-python/README.md)
  - *part 2:* complete rewrite, as brute forcing all paths was no longer
    possible; took some inspiration on how to solve this; the idea is to build
    a graph that connects start, end, and all crossing points; edges of the
    graphs are calculated by traveling the actual maze; an edge may not travel
    through another node; eventually we have a graph where the edges are
    annotated with the path distances between the nodes; to find the longest
    total distance, there is no better way than to try all possible paths (done
    via recursive DFS); my input had 36 nodes and 120 edges for part 2;
    calculation takes about 8 seconds

## [Day 24: Never Tell Me The Odds](https://adventofcode.com/2023/day/24)  🌨️
  - ⭐⭐ [Python](day-24-python/day24.py)
  - *part 1:* implementing some geometry, perfectly test-driven by the elaborate
    examples; learned about
    [homogeneous coordinates](https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Using_homogeneous_coordinates)
  - *part 2:* solved with [Z3](https://github.com/Z3Prover/z3/wiki)

## [Day 25: Snowverload](https://adventofcode.com/2023/day/25) ❄️
  - ⭐⭐ [Python](day-25-python/day25.py)
  - *part 1:* did a lot of reading on graph theory, and my conclusion was that
    we can find a solution based on max flows, which eventually turned out to
    be right; no further spoilers here, those are in the
    [day 25 README](day-25-python/README.md)
    - used [NetworkX](https://networkx.org) for the graph stuff, which felt a
      bit like cheating, but understanding the problem and picking the right
      algorithms was a challenge in itself; also, using NetworkX for the first
      time was interesting, it's a cool library to have in the toolbox
  - *part 2:* n/a (auto-win with all other stars)
