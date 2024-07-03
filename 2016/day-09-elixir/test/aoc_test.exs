defmodule AocTest do
  use ExUnit.Case
  doctest Aoc

  test "part1 works for all the examples" do
    assert Aoc.part1("ADVENT") == 6
    assert Aoc.part1("A(1x5)BC") == 7
    assert Aoc.part1("(3x3)XYZ") == 9
    assert Aoc.part1("A(2x2)BCD(2x2)EFG") == 11
    assert Aoc.part1("(6x1)(1x3)A") == 6
    assert Aoc.part1("X(8x2)(3x3)ABCY") == 18
  end

  test "decompress works for all the examples" do
    assert Aoc.decompress("ADVENT") == "ADVENT"
    assert Aoc.decompress("A(1x5)BC") == "ABBBBBC"
    assert Aoc.decompress("(3x3)XYZ") == "XYZXYZXYZ"
    assert Aoc.decompress("A(2x2)BCD(2x2)EFG") == "ABCBCDEFEFG"
    assert Aoc.decompress("(6x1)(1x3)A") == "(1x3)A"
    assert Aoc.decompress("X(8x2)(3x3)ABCY") == "X(3x3)ABC(3x3)ABCY"
  end

  test "part2 works for all the examples" do
    assert Aoc.part2("(3x3)XYZ") == 9
    assert Aoc.part2("X(8x2)(3x3)ABCY") == 20
    assert Aoc.part2("(27x12)(20x12)(13x14)(7x10)(1x12)A") == 241_920
    assert Aoc.part2("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN") == 445
  end
end
