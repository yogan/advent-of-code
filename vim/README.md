# Advent of Code Vim Solutions

- [2022 Day 01](https://adventofcode.com/2022/day/1) ⭐⭐
    - pretty easy, as we just have to sum up numbers and sort a bit
    - solved by a bunch of Ex commands:
        - [`aoc2201.vim`](2022/day-01/aoc2201.vim)
        - [`aoc2201.commented.vim`](2022/day-01/aoc2201.commented.vim)
    - script file can either be passed to Vim with `-s aoc2201.vim` (see
      [`run.sh`](2022/day-01/run.sh)) or loaded at runtime with
      `:so[urce] aoc2201.vim` (expects an empty buffer and input data in
      `input.txt`)
