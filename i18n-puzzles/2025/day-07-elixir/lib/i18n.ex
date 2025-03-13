# Disable the "redefining module" warning that `mix run` prints for some reason
Code.put_compiler_option(:ignore_module_conflict, true)

defmodule I18n do
  @spec start(String.t()) :: integer()
  def start(input) do
    input
    |> String.trim_trailing()
    |> String.split("\n")
    |> Enum.map(&fix_timezone/1)
    |> Enum.with_index(1)
    |> Enum.reduce(0, fn {dt, i}, acc -> acc + dt.hour * i end)
  end

  def fix_timezone(line) do
    [iso, correct, wrong] = String.split(line, ~r/\s+/)

    {:ok, dt, _} = DateTime.from_iso8601(iso)
    {:ok, naive_dt} = NaiveDateTime.from_iso8601(iso)

    halifax = DateTime.shift_zone!(dt, "America/Halifax") |> DateTime.to_naive()
    santiago = DateTime.shift_zone!(dt, "America/Santiago") |> DateTime.to_naive()

    {datetime, zone} =
      if halifax == naive_dt do
        {halifax, "America/Halifax"}
      else
        {santiago, "America/Santiago"}
      end

    datetime
    |> DateTime.from_naive!(zone)
    |> DateTime.add(String.to_integer(correct), :minute)
    |> DateTime.add(-String.to_integer(wrong), :minute)
  end
end
