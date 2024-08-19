import unittest
include lib

suite "Advent of Code 2016 Day 18: Like a Rogue":

  test "nextRow larger example step 1":
    check(nextRow(".^^.^.^^^^") == "^^^...^..^")

  test "nextRow larger example step 2":
    check(nextRow("^^^...^..^") == "^.^^.^.^^.")

  test "nextRow larger example step 3":
    check(nextRow("^.^^.^.^^.") == "..^^...^^^")

  test "nextRow larger example step 4":
    check(nextRow("..^^...^^^") == ".^^^^.^^.^")

  test "safeTiles works for the larger example":
    check(safeTiles(".^^.^.^^^^", 10) == 38)
