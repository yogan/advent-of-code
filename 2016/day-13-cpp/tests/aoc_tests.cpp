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

TEST_CASE("part1 works for the sample input") {
    REQUIRE(part1(7, 4, 10) == 11);
}

TEST_CASE("part1 works for the real input") {
    REQUIRE(part1(31, 39, 1350) == 92);
}
