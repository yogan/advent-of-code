using AoC
using Test

@testset "parseInput() splits string into array" begin
    @test parseInput("1122") == [1, 1, 2, 2]
    @test parseInput("1111") == [1, 1, 1, 1]
    @test parseInput("1234") == [1, 2, 3, 4]
    @test parseInput("91212129") == [9, 1, 2, 1, 2, 1, 2, 9]
end

@testset "part1() returns correct values for samples" begin
    @test part1(parseInput("1122")) == 3
    @test part1(parseInput("1111")) == 4
    @test part1(parseInput("1234")) == 0
    @test part1(parseInput("91212129")) == 9
end

@testset "part2() returns correct values for samples" begin
    @test part2(parseInput("1212")) == 6
    @test part2(parseInput("1221")) == 0
    @test part2(parseInput("123425")) == 4
    @test part2(parseInput("123123")) == 12
    @test part2(parseInput("12131415")) == 4
end
