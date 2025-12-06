# Disable the "redefining module" warning that `mix run` prints for some reason
Code.put_compiler_option(:ignore_module_conflict, true)

defmodule Aoc do
  @typedoc "Range tuple: {low, high}"
  @type range :: {integer(), integer()}

  @spec part1(list(range()), list(integer())) :: integer()
  def part1(ranges, ids) do
    Enum.count(ids, fn id ->
      Enum.any?(ranges, fn {lo, hi} -> lo <= id and id <= hi end)
    end)
  end

  @spec part2(list(range())) :: integer()
  def part2(ranges) do
    ranges
    |> Enum.sort()
    |> then(fn [first | rest] ->
      Enum.reduce(rest, {first, 0}, fn next, {cur, counter} ->
        if overlap?(cur, next) do
          {merge(cur, next), counter}
        else
          {next, counter + size(cur)}
        end
      end)
    end)
    |> then(fn {final, counter} -> counter + size(final) end)
  end

  defp overlap?({_, hi1}, {lo2, _}), do: lo2 <= hi1
  defp merge({lo, hi1}, {_, hi2}), do: {lo, max(hi1, hi2)}
  defp size({lo, hi}), do: hi - lo + 1

  def start(input) do
    {ranges, ids} = parse(input)
    part1(ranges, ids) |> IO.puts()
    part2(ranges) |> IO.puts()
  end

  @spec parse(String.t()) :: {list(range()), list(integer())}
  defp parse(line) do
    line
    |> String.trim()
    |> String.split("\n\n")
    |> Enum.map(&String.split(&1, "\n"))
    |> then(fn [ranges, ids] ->
      {
        ranges
        |> Enum.map(fn range ->
          range
          |> String.split("-")
          |> Enum.map(&String.to_integer/1)
          |> List.to_tuple()
        end),
        ids |> Enum.map(&String.to_integer/1)
      }
    end)
  end
end
