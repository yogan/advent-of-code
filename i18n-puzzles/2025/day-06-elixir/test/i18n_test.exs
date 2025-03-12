defmodule I18nTest do
  use ExUnit.Case
  doctest I18n

  test "mojibake is fixed" do
    assert I18n.fix_mojibake("religiÃ«n") == "religiën"
    assert I18n.fix_mojibake("kÃ¼rst") == "kürst"
  end

  test "double mojibake is fixed" do
    assert I18n.fix_mojibake(I18n.fix_mojibake("pugilarÃÂ£o")) == "pugilarão"
  end

  test "crossword puzzle can be solved" do
    words = [
      {"geléet", 1},
      {"träffs", 2},
      {"religiën", 3},
      {"tancées", 4},
      {"kürst", 5},
      {"roekoeën", 6},
      {"skälen", 7},
      {"böige", 8},
      {"fägnar", 9},
      {"dardées", 10},
      {"amènent", 11},
      {"orquestrá", 12},
      {"imputarão", 13},
      {"molières", 14},
      {"pugilarão", 15},
      {"azeitámos", 16},
      {"dagcrème", 17},
      {"zöger", 18},
      {"ondulât", 19},
      {"blökt", 20}
    ]

    puzzle = [
      "...d...",
      "..e.....",
      ".l...",
      "....f.",
      "......t.."
    ]

    #       |
    #    darDées   (10)
    #     roEkoeën (6)
    #      bLökt   (20)
    #   träfFs     (2)
    # orquesTrá    (12)
    #       |
    assert I18n.solve_crossword(puzzle, words) == [12, 2, 20, 6, 10]
  end
end
