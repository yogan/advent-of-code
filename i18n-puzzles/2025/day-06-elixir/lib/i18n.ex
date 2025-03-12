# Disable the "redefining module" warning that `mix run` prints for some reason
Code.put_compiler_option(:ignore_module_conflict, true)

defmodule I18n do
  @spec start(String.t()) :: integer()
  def start(input) do
    [top, bottom] = input |> String.split("\n\n")

    words =
      top
      |> String.split("\n")
      |> Enum.with_index()
      |> Enum.map(fn {word, i} -> {fix(word, i + 1), i + 1} end)

    puzzle =
      bottom
      |> String.split("\n")
      |> Enum.map(&String.trim/1)

    solve_crossword(puzzle, words) |> Enum.sum()
  end

  @spec fix(String.t(), integer()) :: String.t()
  defp fix(word, i) do
    case {rem(i, 3), rem(i, 5), rem(i, 15)} do
      {0, 0, _} -> fix_mojibake(fix_mojibake(word))
      {0, _, _} -> fix_mojibake(word)
      {_, 0, _} -> fix_mojibake(word)
      {_, _, _} -> word
    end
  end

  @spec fix_mojibake(String.t()) :: String.t()
  def fix_mojibake(input) do
    input
    |> :unicode.characters_to_binary(:utf8, :latin1)
    |> :unicode.characters_to_list(:utf8)
    |> List.to_string()
  end

  @spec solve_crossword([String.t()], [{String.t(), integer()}]) :: [integer()]
  def solve_crossword(puzzle, words) do
    solve_rec(puzzle, words, [])
  end

  defp solve_rec(puzzle, words, res) do
    case puzzle do
      [] ->
        res

      [line | rest] ->
        line_number =
          words
          |> Enum.filter(fn {word, _} -> matches(word, line) end)
          |> List.first()
          |> Tuple.to_list()
          |> Enum.at(1)

        solve_rec(rest, words, [line_number | res])
    end
  end

  defp matches(word, pattern) do
    [{idx, _}] = Regex.run(~r/[^.]/, pattern, return: :index, capture: :first)

    String.length(word) == String.length(pattern) &&
      String.at(word, idx) == String.at(pattern, idx)
  end
end
