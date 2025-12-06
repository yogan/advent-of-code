using AoC
using Test

sample = [
    "123 328  51 64 \n",
    " 45 64  387 23 \n",
    "  6 98  215 314\n",
    "*   +   *   +  \n"]

@testset "part1() works for the sample" begin
    @test part1(sample) == 4277556
end

@testset "part2() works for the sample" begin
    @test part2(sample) == 3263827
end
