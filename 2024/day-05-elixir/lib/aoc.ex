# Disable the "redefining module" warning that `mix run` prints for some reason
Code.put_compiler_option(:ignore_module_conflict, true)

defmodule Aoc do
  def start(input) do
    {rules, updates} = parse(input)
    {valid, invalid} = Enum.split_with(updates, &is_valid(rules, &1))

    valid |> sum_middle_pages() |> IO.puts()
    invalid |> Enum.map(&sort(rules, &1)) |> sum_middle_pages() |> IO.puts()
  end

  @type rule :: {integer(), integer()}
  @type update :: list(integer())

  @spec parse(String.t()) :: {list(rule), list(update)}
  def parse(input) do
    [rules, updates] =
      input
      |> String.trim_trailing()
      |> String.split("\n\n")
      |> Enum.map(&String.split(&1, "\n"))

    rules =
      rules
      |> Enum.map(&String.split(&1, "|"))
      |> Enum.map(&{String.to_integer(Enum.at(&1, 0)), String.to_integer(Enum.at(&1, 1))})

    updates =
      updates
      |> Enum.map(&String.split(&1, ","))
      |> Enum.map(&Enum.map(&1, fn x -> String.to_integer(x) end))

    {rules, updates}
  end

  @spec is_valid(list(rule()), update()) :: boolean()
  def is_valid(rules, update) do
    Enum.all?(rules, fn {left, right} ->
      i = Enum.find_index(update, &(&1 == left))
      j = Enum.find_index(update, &(&1 == right))
      i == nil or j == nil or i < j
    end)
  end

  @spec sum_middle_pages(list(update())) :: integer()
  def sum_middle_pages(updates) do
    updates
    |> Enum.map(&Enum.at(&1, div(Enum.count(&1), 2)))
    |> Enum.sum()
  end

  @spec sort(list(rule()), update()) :: update()
  def sort(rules, update) do
    Enum.sort_by(
      update,
      & &1,
      &Enum.member?(rules, {&1, &2})
    )
  end
end
