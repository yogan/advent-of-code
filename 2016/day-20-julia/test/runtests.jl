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

@testset "countUnblocked() returns the number of unblocked numbers" begin
    # [3;3] and [9;4294967295] are free
    @test countUnblocked(Set([Range(5, 8), Range(0, 2), Range(4, 7)])) ==
          (3 - 3 + 1) + (4294967295 - 9 + 1)

    # [0;3562619187] and [3566180939;4294967295] are free
    @test countUnblocked(Set([Range(3562619188, 3566180938)])) ==
          (3562619187 - 0 + 1) + (4294967295 - 3566180939 + 1)

    # [0;9] is free
    @test countUnblocked(Set([Range(10, 4294967295)])) == 10
    #
    # [0;9] is free
    @test countUnblocked(Set([Range(17, 23), Range(10, 4294967295)])) == 10
end
