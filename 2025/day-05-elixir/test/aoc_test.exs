defmodule AocTest do
  use ExUnit.Case
  doctest Aoc

  @ranges [{3, 5}, {10, 14}, {16, 20}, {12, 18}]
  @ids [1, 5, 8, 11, 17, 32]

  test "part1 works for the sample" do
    assert Aoc.part1(@ranges, @ids) == 3
  end

  test "part2 works for the sample" do
    assert Aoc.part2(@ranges) == 14
  end
end
