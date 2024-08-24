include("AoC.jl")
using .AoC

filename = ARGS[1]
lines = readlines(filename)
boxes = parseInput(lines)

println(part1(boxes))
println(part2(boxes))
