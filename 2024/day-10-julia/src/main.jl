include("AoC.jl")
using .AoC
grid = parseInput(readlines(ARGS[1]))
snrs = [score_and_rating(grid, head) for head in trailheads(grid)]
println(sum(s for (s, _) in snrs))
println(sum(r for (_, r) in snrs))
