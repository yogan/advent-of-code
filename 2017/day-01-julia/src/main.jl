include("AoC.jl")
using .AoC

filename = ARGS[1]
digits = parseInput(readline(filename))

println(part1(digits))
println(part2(digits))
