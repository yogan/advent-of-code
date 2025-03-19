defmodule I18nTest do
  use ExUnit.Case
  doctest I18n

  test "hex strings can be parsed" do
    assert I18n.to_hex("616e77c3a4686c65") ==
             [0x61, 0x6E, 0x77, 0xC3, 0xA4, 0x68, 0x6C, 0x65]
  end

  test "utf-8 can be decoded" do
    assert I18n.decode("616e77c3a4686c65") == "anwähle"
  end

  test "latin-1 can be decoded" do
    assert I18n.decode("796c74e46de47373e4") == "yltämässä"
  end

  test "utf-8 with BOM can be decoded" do
    assert I18n.decode("efbbbf73796b6b696dc3a46bc3b6") == "sykkimäkö"
  end

  test "utf-16 big endian can be decoded" do
    assert I18n.decode("0070006f00eb0065006d") == "poëem"
  end

  test "utf-16 big endian with BOM can be decoded" do
    assert I18n.decode("feff0069007400e4007000e400e4006800e4006e") ==
             "itäpäähän"
  end

  test "utf-16 little endian can be decoded" do
    assert I18n.decode("6500700069007400e100660069006f00") == "epitáfio"
  end

  test "full sample can be decoded" do
    [
      "616e77c3a4686c65",
      "796c74e46de47373e4",
      "efbbbf73796b6b696dc3a46bc3b6",
      "0070006f00eb0065006d",
      "feff0069007400e4007000e400e4006800e4006e",
      "61757373e46774",
      "626c6173c3a9",
      "637261776cc3a9",
      "6c00e20063006800e2007400",
      "64657370656e68e1",
      "6c6964e172656973",
      "fffe6700e20063006800e9006500",
      "6500700069007400e100660069006f00",
      "feff007300fc006e006400650072006e",
      "fffe7200f600730074006900"
    ]
    |> Enum.map(&I18n.decode/1)
    |> assert([
      "anwähle",
      "yltämässä",
      "sykkimäkö",
      "poëem",
      "itäpäähän",
      "aussägt",
      "blasé",
      "crawlé",
      "lâchât",
      "despenhá",
      "lidáreis",
      "gâchée",
      "epitáfio",
      "sündern",
      "rösti"
    ])
  end

  test "crossword puzzle can be solved" do
    words = [
      {"anwähle", 1},
      {"yltämässä", 2},
      {"sykkimäkö", 3},
      {"poëem", 4},
      {"itäpäähän", 5},
      {"aussägt", 6},
      {"blasé", 7},
      {"crawlé", 8},
      {"lâchât", 9},
      {"despenhá", 10},
      {"lidáreis", 11},
      {"gâchée", 12},
      {"epitáfio", 13},
      {"sündern", 14},
      {"rösti", 15}
    ]

    puzzle = [
      "   ..s....",
      "  ...w..",
      " ....i",
      ".....f..",
      "    .t......."
    ]

    #       |
    #     aussägt     (6)
    #    crawlé       (8)
    #   rösti        (15)
    #  epitáfio      (13)
    #      itäpäähän  (5)
    #       |
    assert I18n.solve_crossword(puzzle, words) == [5, 13, 15, 8, 6]
  end
end
