defmodule I18nTest do
  use ExUnit.Case
  doctest I18n

  test "timezones are fixed for the sample" do
    assert I18n.fix_timezone("2012-11-05T09:39:00.000-04:00 969 3358")
           |> DateTime.to_iso8601() == "2012-11-03T18:50:00.000-03:00"

    assert I18n.fix_timezone("2012-05-27T17:38:00.000-04:00 2771 246")
           |> DateTime.to_iso8601() == "2012-05-29T11:43:00.000-04:00"

    assert I18n.fix_timezone("2001-01-15T22:27:00.000-03:00 2186 2222")
           |> DateTime.to_iso8601() == "2001-01-15T21:51:00.000-03:00"

    assert I18n.fix_timezone("2017-05-15T07:23:00.000-04:00 2206 4169")
           |> DateTime.to_iso8601() == "2017-05-13T23:40:00.000-03:00"

    assert I18n.fix_timezone("2005-09-02T06:15:00.000-04:00 1764 794")
           |> DateTime.to_iso8601() == "2005-09-02T22:25:00.000-04:00"

    assert I18n.fix_timezone("2008-03-23T05:02:00.000-03:00 1139 491")
           |> DateTime.to_iso8601() == "2008-03-23T15:50:00.000-03:00"

    assert I18n.fix_timezone("2016-03-11T00:31:00.000-04:00 4175 763")
           |> DateTime.to_iso8601() == "2016-03-13T10:23:00.000-03:00"

    assert I18n.fix_timezone("2015-08-14T12:40:00.000-03:00 3697 568")
           |> DateTime.to_iso8601() == "2015-08-16T16:49:00.000-03:00"

    assert I18n.fix_timezone("2013-11-03T07:56:00.000-04:00 402 3366")
           |> DateTime.to_iso8601() == "2013-11-01T07:32:00.000-03:00"

    assert I18n.fix_timezone("2010-04-16T09:32:00.000-04:00 3344 2605")
           |> DateTime.to_iso8601() == "2010-04-16T21:51:00.000-04:00"
  end
end
