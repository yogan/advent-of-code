# Disable the "redefining module" warning that `mix run` prints for some reason
Code.put_compiler_option(:ignore_module_conflict, true)

defmodule Aoc do
  def start(filename) do
    case File.read(filename) do
      {:ok, content} ->
        line = String.trim_trailing(content)
        part1(line) |> IO.puts()
        part2(line) |> IO.puts()

      {:error, reason} ->
        IO.puts("Error reading input file \"#{filename}\" (#{reason})")
        System.halt(1)
    end
  end

  @spec part1(String.t()) :: integer()
  def part1(line) do
    line |> decompress() |> String.length()
  end

  @spec part2(String.t()) :: integer()
  def part2(line) do
    line |> decompress_v2()
  end

  def decompress(line) do
    case String.split(line, ["(", ")"], parts: 3) do
      [left, marker, right] -> left <> repeat(marker, right)
      _ -> line
    end
  end

  defp repeat(marker, str) do
    [len, times] = marker |> String.split("x") |> Enum.map(&String.to_integer/1)
    {left, right} = String.split_at(str, len)

    String.duplicate(left, times) <> decompress(right)
  end

  def decompress_v2(line) do
    case String.split(line, ["(", ")"], parts: 3) do
      [left, marker, right] -> String.length(left) + repeat_v2(marker, right)
      _ -> String.length(line)
    end
  end

  defp repeat_v2(marker, str) do
    [len, times] = marker |> String.split("x") |> Enum.map(&String.to_integer/1)
    {left, right} = String.split_at(str, len)

    times * decompress_v2(left) + decompress_v2(right)
  end
end
