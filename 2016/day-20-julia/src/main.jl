include("AoC.jl")
using .AoC

filename = ARGS[1]
lines = readlines(filename)
blocked = parseInput(lines)

println(part1(blocked))
println(part2(blocked))
