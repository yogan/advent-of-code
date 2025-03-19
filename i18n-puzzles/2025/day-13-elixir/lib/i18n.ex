# Disable the "redefining module" warning that `mix run` prints for some reason
Code.put_compiler_option(:ignore_module_conflict, true)

defmodule I18n do
  @spec start(String.t()) :: integer()
  def start(input) do
    [top, bottom] = input |> String.trim() |> String.split("\n\n")

    words =
      top
      |> String.split("\n")
      |> Enum.with_index()
      |> Enum.map(fn {word, i} -> {decode(word), i + 1} end)

    puzzle =
      bottom
      |> String.split("\n")
      |> Enum.map(&String.trim/1)

    solve_crossword(puzzle, words) |> Enum.sum()
  end

  @spec to_hex(String.t()) :: [integer()]
  def to_hex(input) do
    input
    |> String.split("", trim: true)
    |> Enum.chunk_every(2)
    |> Enum.map(&Enum.join(&1))
    |> Enum.map(&String.to_integer(&1, 16))
  end

  # madness begins here… this is horrible… but… it… w̷͝ͅơ̵̂̈́͜r̵̥͕̐̓k̵̰̀ś̸̯̣̐͜͝

  @spec decode(String.t()) :: String.t()
  def decode(input) do
    case to_hex(input) do
      [0xFE, 0xFF | tail] ->
        parse_as_utf16_big_endian(tail)
        |> Enum.filter(&(&1 != 0))
        |> List.to_string()

      [0xFF, 0xFE | tail] ->
        parse_as_utf16_little_endian(tail)
        |> Enum.filter(&(&1 != 0))
        |> List.to_string()

      [0xEF, 0xBB, 0xBF | tail] ->
        parse_as_utf8(tail) |> List.to_string()

      hex ->
        [
          parse_as_utf16_big_endian(hex),
          parse_as_utf16_little_endian(hex),
          parse_as_utf8(hex),
          parse_as_latin1(hex)
        ]
        |> Enum.map(&unwrap/1)
        |> Enum.filter(&has_only_letters?/1)
        |> List.first()
    end
  end

  defp unwrap(res) do
    case res do
      {:error, _, _} -> "!error!"
      {:incomplete, _, _} -> "!incomplete!"
      _ -> res |> Enum.filter(&(&1 != 0)) |> List.to_string()
    end
  end

  defp has_only_letters?(input) do
    Regex.match?(~r/^\p{L}+$/u, input)
  end

  defp parse_as_utf16_big_endian(input) do
    input
    |> :unicode.characters_to_list({:utf16, :big})
  end

  defp parse_as_utf16_little_endian(input) do
    input
    |> :unicode.characters_to_binary({:utf16, :little})
    |> :unicode.characters_to_list(:utf8)
  end

  defp parse_as_utf8(input) do
    input
    |> :unicode.characters_to_binary(:utf8, :latin1)
    |> :unicode.characters_to_list(:utf8)
  end

  defp parse_as_latin1(input) do
    input
    |> :unicode.characters_to_binary(:latin1, :utf8)
    |> :unicode.characters_to_list(:utf8)
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
          |> Enum.filter(fn {word, _} -> matches(word, String.trim(line)) end)
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
