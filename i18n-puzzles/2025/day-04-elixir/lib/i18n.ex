# Disable the "redefining module" warning that `mix run` prints for some reason
Code.put_compiler_option(:ignore_module_conflict, true)

defmodule I18n do
  @spec start(String.t()) :: integer()
  def start(input) do
    input
    |> String.trim_trailing()
    |> String.split("\n\n")
    |> Enum.map(&String.split(&1, "\n"))
    |> Enum.map(&diff(Enum.at(&1, 0), Enum.at(&1, 1)))
    |> Enum.sum()
  end

  @spec diff(String.t(), String.t()) :: integer()
  def diff(departure, arrival) do
    DateTime.diff(parse(arrival), parse(departure), :minute)
  end

  @spec parse(String.t()) :: DateTime.t()
  def parse(line) do
    [_, tz, date] = Regex.scan(~r/.*:\s*(\S*)\s+(.*)/, line) |> List.first()
    {:ok, naive_dt} = Datix.NaiveDateTime.parse(date, "%b %d, %Y, %H:%M")
    DateTime.from_naive!(naive_dt, tz)
  end
end
