using AoC
using Test

@testset "parseInput() returns a 2d array" begin
    @test parseInput([
        "0123",
        "1234",
        "8765",
        "9876",
    ]) == [
        [0, 1, 2, 3],
        [1, 2, 3, 4],
        [8, 7, 6, 5],
        [9, 8, 7, 6],
    ]
end

sample = parseInput([
    "89010123",
    "78121874",
    "87430965",
    "96549874",
    "45678903",
    "32019012",
    "01329801",
    "10456732",
])

@testset "trailheads() returns coordinates of 0s" begin
    @test trailheads(sample) == [(1, 3), (1, 5), (3, 5), (5, 7), (6, 3), (6, 6),
        (7, 1), (7, 7), (8, 2)]
end

@testset "neighbors() returns coordinates of neighbors" begin
    @test neighbors((1, 1), 8) == Set([(1, 2), (2, 1)])
    @test neighbors((1, 2), 8) == Set([(1, 1), (1, 3), (2, 2)])
    @test neighbors((1, 8), 8) == Set([(1, 7), (2, 8)])
    @test neighbors((2, 1), 8) == Set([(1, 1), (3, 1), (2, 2)])
    @test neighbors((2, 2), 8) == Set([(1, 2), (3, 2), (2, 1), (2, 3)])
    @test neighbors((8, 7), 8) == Set([(8, 6), (8, 8), (7, 7)])
    @test neighbors((8, 8), 8) == Set([(7, 8), (8, 7)])
end

@testset "score_and_rating() returns score and rating" begin
    @test score_and_rating(sample, (1, 3)) == (5, 20)
    @test score_and_rating(sample, (1, 5)) == (6, 24)
    @test score_and_rating(sample, (3, 5)) == (5, 10)
    @test score_and_rating(sample, (5, 7)) == (3, 4)
    @test score_and_rating(sample, (6, 3)) == (1, 1)
    @test score_and_rating(sample, (6, 6)) == (3, 4)
    @test score_and_rating(sample, (7, 1)) == (5, 5)
    @test score_and_rating(sample, (7, 7)) == (3, 8)
    @test score_and_rating(sample, (8, 2)) == (5, 5)
end
