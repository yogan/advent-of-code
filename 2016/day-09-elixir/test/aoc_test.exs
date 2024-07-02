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
end
