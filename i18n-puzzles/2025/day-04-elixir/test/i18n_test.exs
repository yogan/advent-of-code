defmodule I18nTest do
  use ExUnit.Case
  doctest I18n

  test "parse extracts dates with timezones" do
    assert I18n.parse("Departure: Europe/London Mar 04, 2020, 10:00") ==
             DateTime.from_naive!(~N[2020-03-04T10:00:00], "Europe/London")

    assert I18n.parse("Arrival:   Europe/Paris    Mar 04, 2020, 11:59") ==
             DateTime.from_naive!(~N[2020-03-04T11:59:00], "Europe/Paris")

    assert I18n.parse("Departure: America/Argentina/Buenos_Aires Mar 07, 2020, 06:06") ==
             DateTime.from_naive!(~N[2020-03-07T06:06:00], "America/Argentina/Buenos_Aires")
  end

  test "diff calculates time in minutes to travel, considering TZ and DST" do
    assert I18n.diff(
             "Departure: Europe/London                  Mar 04, 2020, 10:00",
             "Arrival:   Europe/Paris                   Mar 04, 2020, 11:59"
           ) == 59

    assert I18n.diff(
             "Departure: Europe/Paris                   Mar 05, 2020, 10:42",
             "Arrival:   Australia/Adelaide             Mar 06, 2020, 16:09"
           ) == 1197

    assert I18n.diff(
             "Departure: Australia/Adelaide             Mar 06, 2020, 19:54",
             "Arrival:   America/Argentina/Buenos_Aires Mar 06, 2020, 19:10"
           ) == 766

    assert I18n.diff(
             "Departure: America/Argentina/Buenos_Aires Mar 07, 2020, 06:06",
             "Arrival:   America/Toronto                Mar 07, 2020, 14:43"
           ) == 637

    assert I18n.diff(
             "Departure: America/Toronto                Mar 08, 2020, 04:48",
             "Arrival:   Europe/London                  Mar 08, 2020, 16:52"
           ) == 484
  end
end
