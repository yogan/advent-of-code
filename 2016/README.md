# [Advent of Code 2016](https://adventofcode.com/2016)

- [Day 01](https://adventofcode.com/2016/day/1) ⭐⭐ in
  [Vim](../vim/2016/day-01/aoc-2016-01.vim)
  ([commented](../vim/2016/day-01/aoc-2016-01.commented.vim))
- [Day 02](https://adventofcode.com/2016/day/2) ⭐⭐ in
  [Vim](../vim/2016/day-02/aoc-2016-02.vim)
  ([commented](../vim/2016/day-02/aoc-2016-02.commented.vim))
- [Day 03](https://adventofcode.com/2016/day/3) ⭐⭐ in
  [Vim](../vim/2016/day-03/aoc-2016-03.vim)
  ([commented](../vim/2016/day-03/aoc-2016-03.commented.vim))
- [Day 04](https://adventofcode.com/2016/day/4) ⭐⭐ in
  [Vim](../vim/2016/day-04/aoc-2016-04.vim)
  ([commented](../vim/2016/day-04/aoc-2016-04.commented.vim))
- [Day 05](https://adventofcode.com/2016/day/5) ⭐⭐ in
  - [Gleam](./day-05-gleam/src/aoc_2016_day_05.gleam)
    ([tests](./day-05-gleam/test/aoc_2016_day_05_test.gleam))
  - [Rust](./day-05-rust/src/main.rs)
  - I was curious if Rust would be faster than Gleam for this problem. It was,
    but not much. Gleam runs ~26 sec, while Rust runs ~19 sec. It probably comes
    down to how performant the MD5 implementations are.
- [Day 06](https://adventofcode.com/2016/day/6) ⭐⭐ in
  [Vim](../vim/2016/day-06/aoc-2016-06.vim)
  ([commented](../vim/2016/day-06/aoc-2016-06.commented.vim))
- [Day 07](https://adventofcode.com/2016/day/7) ⭐⭐ in Haskell
  ([Main.hs](./day-07-haskell/app/Main.hs),
  **[Lib.hs](./day-07-haskell/src/Lib.hs)**,
  [Spec.hs](./day-07-haskell/test/Spec.hs))
- [Day 08](https://adventofcode.com/2016/day/8) ⭐⭐ in
  [Vim](../vim/2016/day-08/aoc-2016-08.vim)
  ([commented](../vim/2016/day-08/aoc-2016-08.commented.vim)) -
  [animated solution](../vim/2016/day-08/README.md)
- [Day 09](https://adventofcode.com/2016/day/9) ⭐⭐ in Elixir
  (**[aoc.ex](./day-09-elixir/lib/aoc.ex)**,
  [main.ex](./day-09-elixir/lib/main.ex),
  [aoc_test.exs](./day-09-elixir/test/aoc_test.exs))
- [Day 10](https://adventofcode.com/2016/day/10) ⭐⭐ in Kotlin
  (**[App.kt](./day-10-kotlin/app/src/main/kotlin/aoc/App.kt)**,
  [AppTest.kt](./day-10-kotlin/app/src/test/kotlin/aoc/AppTest.kt))
- [Day 11](https://adventofcode.com/2016/day/11) ⭐⭐ in TypeScript (Bun)
  (**[aoc.ts](./day-11-typescript/aoc.ts)**,
  [aoc.test.ts](./day-11-typescript/aoc.test.ts))
  - the first really challenging problem of 2016
  - BFS approach with seen states skipping
  - initially by serializing states and comparing them, which was fine for part
    1, but way too slow for part 2
  - optimization for part 2 was to treat states as structurally identical -
    swapping single items or pairs between the floors would not matter for the
    number of required steps
- [Day 12](https://adventofcode.com/2016/day/12) ⭐⭐ in Crystal
  (**[aoc.cr](./day-12-crystal/src/aoc.cr)**,
  [aoc_spec.cr](./day-12-crystal/spec/aoc_spec.cr))
  - straight-forward mini assembly interpreter
  - a language with `eval` would have reduce the code a lot…
- [Day 13](https://adventofcode.com/2016/day/13) ⭐⭐ in C++
  (**[aoc.cpp](./day-13-cpp/src/aoc.cpp)**,
  [aoc_tests.cpp](./day-13-cpp/tests/aoc_tests.cpp),
  [main.cpp](./day-13-cpp/src/main.cpp))
  - flood fill algorithm
  - using [`std::bitset`](https://en.cppreference.com/w/cpp/utility/bitset) for
    visited positions
- [Day 14](https://adventofcode.com/2016/day/14) ⭐⭐ in Python
  ([aoc.py](./day-14-python/aoc.py))
  - using [`@functools.lru_cache`](https://docs.python.org/3/library/functools.html#functools.lru_cache)
