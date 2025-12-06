include("AoC.jl")
using .AoC

readlines(ARGS[1]) |>
parseInput |>
(boxes -> println.((part1(boxes), part2(boxes))))
