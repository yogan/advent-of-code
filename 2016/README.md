# [Advent of Code 2016](https://adventofcode.com/2016)

- [Day 01](https://adventofcode.com/2016/day/1) ⭐⭐ in
  [Vim](../vim/2016/day-01/aoc.vim)
- [Day 02](https://adventofcode.com/2016/day/2) ⭐⭐ in
  [Vim](../vim/2016/day-02/aoc.vim)
- [Day 03](https://adventofcode.com/2016/day/3) ⭐⭐ in
  [Vim](../vim/2016/day-03/aoc.vim)
- [Day 04](https://adventofcode.com/2016/day/4) ⭐⭐ in
  [Vim](../vim/2016/day-04/aoc.vim)
- [Day 05](https://adventofcode.com/2016/day/5) ⭐⭐ in
  - [Gleam](./day-05-gleam/src/aoc_2016_day_05.gleam)
    ([tests](./day-05-gleam/test/aoc_2016_day_05_test.gleam))
  - [Rust](./day-05-rust/src/main.rs)
  - I was curious if Rust would be faster than Gleam for this problem. It was,
    but not much. Gleam runs ~26 sec, while Rust runs ~19 sec. It probably comes
    down to how performant the MD5 implementations are.
- [Day 06](https://adventofcode.com/2016/day/6) ⭐⭐ in
  [Vim](../vim/2016/day-06/aoc.vim)
- [Day 07](https://adventofcode.com/2016/day/7) ⭐⭐ in Haskell
  ([Main.hs](./day-07-haskell/app/Main.hs),
  [Lib.hs](./day-07-haskell/src/Lib.hs),
  [Spec.hs](./day-07-haskell/test/Spec.hs))
- [Day 08](https://adventofcode.com/2016/day/8) ⭐⭐ in
  [Vim](../vim/2016/day-08/aoc.vim)
  [animated solution](../vim/2016/day-08/README.md)
- [Day 09](https://adventofcode.com/2016/day/9) ⭐⭐ in Elixir
  ([aoc.ex](./day-09-elixir/lib/aoc.ex),
  [main.ex](./day-09-elixir/lib/main.ex),
  [aoc_test.exs](./day-09-elixir/test/aoc_test.exs))
- [Day 10](https://adventofcode.com/2016/day/10) ⭐⭐ in Kotlin
  ([App.kt](./day-10-kotlin/app/src/main/kotlin/aoc/App.kt),
  [AppTest.kt](./day-10-kotlin/app/src/test/kotlin/aoc/AppTest.kt))
- [Day 11](https://adventofcode.com/2016/day/11) ⭐⭐ in TypeScript (Bun)
  ([aoc.ts](./day-11-typescript/aoc.ts),
  [aoc.test.ts](./day-11-typescript/aoc.test.ts))
  - the first really challenging problem of 2016
  - BFS approach with seen states skipping
  - initially by serializing states and comparing them, which was fine for part
    1, but way too slow for part 2
  - optimization for part 2 was to treat states as structurally identical -
    swapping single items or pairs between the floors would not matter for the
    number of required steps
- [Day 12](https://adventofcode.com/2016/day/12) ⭐⭐ in Crystal
  ([aoc.cr](./day-12-crystal/src/aoc.cr),
  [aoc_spec.cr](./day-12-crystal/spec/aoc_spec.cr))
  - straight-forward mini assembly interpreter
  - a language with `eval` would have reduce the code a lot…
- [Day 13](https://adventofcode.com/2016/day/13) ⭐⭐ in C++
  ([aoc.cpp](./day-13-cpp/src/aoc.cpp),
  [aoc_tests.cpp](./day-13-cpp/tests/aoc_tests.cpp),
  [main.cpp](./day-13-cpp/src/main.cpp))
  - flood fill algorithm
  - using [`std::bitset`](https://en.cppreference.com/w/cpp/utility/bitset) for
    visited positions
- [Day 14](https://adventofcode.com/2016/day/14) ⭐⭐ in Python
  ([aoc.py](./day-14-python/aoc.py))
  - using [`@functools.lru_cache`](https://docs.python.org/3/library/functools.html#functools.lru_cache)
- [Day 15](https://adventofcode.com/2016/day/15) ⭐⭐ in
  [Vim](../vim/2016/day-15/aoc.vim)
  - the puzzle is pretty much exactly the [Chinese Remainder
    Theorem](https://en.wikipedia.org/wiki/Chinese_remainder_theorem)
  - initially solved by hand, see [Day 15 README](../vim/2016/day-15/README.md)
- [Day 16](https://adventofcode.com/2016/day/16) ⭐⭐
  - Clojure
    ([core.clj](./day-16-clojure/src/advent_of_code_template/core.clj),
    [core_test.clj](./day-16-clojure/test/advent_of_code_template/core_test.clj))
    – runtime ~ 1 minute
  - Rust ([main.rs](./day-16-rust/src/main.rs)) – runtime < 1 second
  - both solutions use the same algorithm (basically doing exactly what the
    puzzle asks for: creating the whole data string and then calculating the
    checksum), but the whole list creation stuff seems to be very slow in
    Clojure (probably due to me limited knowledge of the language)
- [Day 17](https://adventofcode.com/2016/day/17) ⭐⭐ in C#
  ([AoC.cs](./day-17-csharp/AoC.cs),
  [AoCTests.cs](./day-17-csharp/AoCTests.cs),
  [Program.cs](./day-17-csharp/Program.cs))
  - BFS
  - caching MD5 hashes does not improve performance, as the hashed strings
    include the path taken, which is different for each step of the BFS
  - runtime is good enough anyway (< 2 sec)
- [Day 18](https://adventofcode.com/2016/day/18) ⭐⭐ in Nim
  ([lib.nim](./day-18-nim/lib.nim),
  [tests.nim](./day-18-nim/tests.nim),
  [main.nim](./day-18-nim/main.nim))
  - surprisingly easy puzzle for day 18
  - very naïve solution, but part 2 runs in ~ 6 sec
- [Day 19](https://adventofcode.com/2016/day/19) ⭐⭐ in
  [Zig](./day-19-zig/aoc.zig)
  - part 2 was a struggle, eventually built my own circular double-linked list
  - the trick to get the runtime from hours to basically instant was to have a
    pointer to the opposite of the circle, and moving that forward either one or
    two places, depending on the oddness of the remaining elves
- [Day 20](https://adventofcode.com/2016/day/20) ⭐⭐ in Julia
  ([`AoC.jl`](./day-20-julia/src/AoC.jl),
  [`main.jl`](./day-20-julia/src/main.jl),
  [`runtests.jl`](./day-20-julia/test/runtests.jl))
- [Day 21](https://adventofcode.com/2016/day/21) ⭐⭐ in Perl
  ([`AoC.pm`](./day-21-perl/lib/AoC.pm),
  [`main.pl`](./day-21-perl/main.pl),
  [`tests.t`](./day-21-perl/t/tests.t))
- [Day 22](https://adventofcode.com/2016/day/22) ⭐ in Lua
  ([`aoc.lua`](./day-22-lua/aoc.lua),
  [`aoc_spec.lua`](./day-22-lua/aoc_spec.lua),
  [`main.lua`](./day-22-lua/main.lua))
