#define CATCH_CONFIG_MAIN
#include "../src/aoc.cpp"
#include <catch2/catch_all.hpp>
#include <tuple>

// HINT: for tests where == is not enough, see matchers:
// https://github.com/catchorg/Catch2/blob/devel/docs/matchers.md

TEST_CASE("parse_line reads the box dimensions from a string") {
    REQUIRE(parse_line("1x2x3") == make_tuple(1, 2, 3));
    REQUIRE(parse_line("1x42x9001") == make_tuple(1, 42, 9001));
}

TEST_CASE("part1 returns the sum of the volumes of the boxes") {
    auto boxes = vector<tuple<int, int, int>>{
        make_tuple(1, 2, 3),
        make_tuple(1, 1, 1),
    };
    REQUIRE(part1(boxes) == 6 + 1);
}
