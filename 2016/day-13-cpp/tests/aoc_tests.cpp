#include "catch2/catch_test_macros.hpp"
#define CATCH_CONFIG_MAIN
#include "../src/aoc.cpp"
#include <catch2/catch_all.hpp>

TEST_CASE("printable_maze returns the sample maze") {
    auto favorite_number = 10;

    vector<string> maze = printable_maze(10, 7, favorite_number);

    REQUIRE(maze.size() == 7);
    REQUIRE(maze[0] == ".#.####.##");
    REQUIRE(maze[1] == "..#..#...#");
    REQUIRE(maze[2] == "#....##...");
    REQUIRE(maze[3] == "###.#.###.");
    REQUIRE(maze[4] == ".##..#..#.");
    REQUIRE(maze[5] == "..##....#.");
    REQUIRE(maze[6] == "#...##.###");
}

TEST_CASE("flood_fill works for the sample input") {
    auto [part1, part2] = flood_fill(7, 4, 10);

    REQUIRE(part1 == 11);
    REQUIRE(part2 == 151);
}

TEST_CASE("flood_fill works for the real input") {
    auto [part1, part2] = flood_fill(31, 39, 1350);

    REQUIRE(part1 == 92);
    REQUIRE(part2 == 124);
}
