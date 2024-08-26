using AoC
using Test

@testset "parseInput() returns a list of ranges" begin
    @test parseInput(["5-8", "0-2", "4-7"]) ==
          [Range(5, 8), Range(0, 2), Range(4, 7)]
    @test parseInput(["3562619188-3566180938"]) ==
          [Range(3562619188, 3566180938)]
end

@testset "lowestUnblocked() returns the lowest number not in any range" begin
    @test lowestUnblocked(Set([Range(5, 8), Range(0, 2), Range(4, 7)])) == 3
    @test lowestUnblocked(Set([Range(3562619188, 3566180938)])) == 0
end
