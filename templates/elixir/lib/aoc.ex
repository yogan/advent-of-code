# Disable the "redefining module" warning that `mix run` prints for some reason
Code.put_compiler_option(:ignore_module_conflict, true)

defmodule Aoc do
  def start(filename) do
    case File.read(filename) do
      {:ok, content} ->
        lines =
          content
          |> String.trim_trailing()
          |> String.split("\n")
          |> Enum.map(&parse/1)

        part1(lines) |> IO.puts()
        part2(lines) |> IO.puts()

      {:error, reason} ->
        IO.puts("Error reading input file \"#{filename}\" (#{reason})")
        System.halt(1)
    end
  end

  @type box :: list(integer())

  @spec parse(String.t()) :: box()
  def parse(line) do
    line |> String.split("x") |> Enum.map(&String.to_integer/1)
  end

  @spec part1(list(box())) :: integer()
  def part1(boxes) do
    boxes |> Enum.map(&volume/1) |> Enum.sum()
  end

  @spec part2(list(box())) :: integer()
  def part2(boxes) do
    boxes |> Enum.map(&surface_area/1) |> Enum.sum()
  end

  defp surface_area([l, w, h]) do
    2 * l * w + 2 * w * h + 2 * h * l
  end

  defp volume([l, w, h]) do
    l * w * h
  end
end
