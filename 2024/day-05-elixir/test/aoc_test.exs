defmodule AocTest do
  use ExUnit.Case
  doctest Aoc

  @sample_input """
  47|53
  97|13
  97|61
  97|47
  75|29
  61|13
  75|53
  29|13
  97|29
  53|29
  61|53
  97|53
  61|29
  47|13
  75|47
  97|75
  47|61
  75|61
  47|29
  75|13
  53|13

  75,47,61,53,29
  97,61,53,29,13
  75,29,13
  75,97,47,61,53
  61,13,29
  97,13,75,29,47
  """

  test "sample input is parsed into rules and updates" do
    {rules, updates} = Aoc.parse(@sample_input)

    assert length(rules) == 21
    assert rules |> hd() == {47, 53}
    assert rules |> Enum.at(-1) == {53, 13}

    assert length(updates) == 6
    assert updates |> hd() == [75, 47, 61, 53, 29]
    assert updates |> Enum.at(-1) == [97, 13, 75, 29, 47]
  end

  test "first three updates are valid, last three are invalid" do
    {rules, updates} = Aoc.parse(@sample_input)
    {first, last} = Enum.split(updates, 3)

    assert Enum.all?(first, &Aoc.is_valid(rules, &1))
    assert Enum.all?(last, &(!Aoc.is_valid(rules, &1)))
  end

  test "middle pages of valid updates are summed up" do
    assert Aoc.sum_middle_pages([
             [75, 47, 61, 53, 29],
             [97, 61, 53, 29, 13],
             [75, 29, 13]
           ]) == 61 + 53 + 29
  end

  test "invalid updates can be fixed by sorting the pages" do
    {rules, _} = Aoc.parse(@sample_input)

    assert Aoc.sort(rules, [75, 97, 47, 61, 53]) == [97, 75, 47, 61, 53]
    assert Aoc.sort(rules, [61, 13, 29]) == [61, 29, 13]
    assert Aoc.sort(rules, [97, 13, 75, 29, 47]) == [97, 75, 47, 29, 13]
  end
end
