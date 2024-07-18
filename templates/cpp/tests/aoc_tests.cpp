#define CATCH_CONFIG_MAIN
#include "../src/aoc.cpp"
#include <catch2/catch_all.hpp>

TEST_CASE("add") {
    REQUIRE(add(1, 1) == 2);
    REQUIRE(add(1, 2) == 3);
    REQUIRE(add(2, 2) == 4);
}
