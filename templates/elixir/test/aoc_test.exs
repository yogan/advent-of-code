defmodule AocTest do
  use ExUnit.Case
  doctest Aoc

  test "parse splits a line by 'x'" do
    assert Aoc.parse("1x2x420") == [1, 2, 420]
  end

  @box1 [2, 3, 4]
  @box2 [1, 1, 10]

  test "part1 returns the sum of the volumes of the boxes" do
    volume1 = 2 * 3 * 4
    volume2 = 1 * 1 * 10

    assert Aoc.part1([@box1, @box2]) == volume1 + volume2
  end

  test "part2 returns the sum of the surfaces area of the boxes" do
    surface1 = 2 * (2 * 3 + 3 * 4 + 4 * 2)
    surface2 = 2 * (1 * 1 + 1 * 10 + 10 * 1)

    assert Aoc.part2([@box1, @box2]) == surface1 + surface2
  end
end
