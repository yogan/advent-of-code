include("AoC.jl")
using .AoC

readlines(ARGS[1], keep=true) |>
(lines -> println.((part1(lines), part2(lines))))
