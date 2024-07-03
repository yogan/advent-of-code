# Disable the "redefining module" warning that `mix run` prints for some reason
Code.put_compiler_option(:ignore_module_conflict, true)

defmodule Aoc do
  def start(input) do
    line = String.trim_trailing(input)
    part1(line) |> IO.puts()
    part2(line) |> IO.puts()
  end

  @spec part1(String.t()) :: integer()
  def part1(line) do
    line |> decompress(true)
  end

  @spec part2(String.t()) :: integer()
  def part2(line) do
    line |> decompress(false)
  end

  defp decompress(line, v1) do
    case String.split(line, ["(", ")"], parts: 3) do
      [left, marker, right] -> String.length(left) + repeat(marker, right, v1)
      _ -> String.length(line)
    end
  end

  defp repeat(marker, str, v1) do
    [len, times] = marker |> String.split("x") |> Enum.map(&String.to_integer/1)
    {left, right} = String.split_at(str, len)

    left_len = if v1, do: String.length(left), else: decompress(left, v1)
    times * left_len + decompress(right, v1)
  end
end
