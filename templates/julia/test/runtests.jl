using AoC
using Test

@testset "parseInput() returns a list of boxes" begin
    @test parseInput(["1x2x3", "987x10x1"]) ==
    [Box(1, 2, 3), Box(987, 10, 1)]
end

input = [Box(1, 2, 3), Box(1, 1, 1)]

@testset "part1() returns sum of volumes" begin
    @test part1(input) == 6 + 1
end

@testset "part2() returns sum of surface areas" begin
    @test part2(input) == (2 + 2 + 3 + 3 + 6 + 6) + (6 * 1)
end

