import unittest
from copy import deepcopy
from input import read_and_solve


def parse_input(lines):
    return [list(line) for line in lines]


def step(seafloor):
    height = len(seafloor)
    width = len(seafloor[0])

    seafloor_v2 = deepcopy(seafloor)

    for r, row in enumerate(seafloor):
        for c, location in enumerate(row):
            if location == ">":
                if seafloor[r][(c + 1) % width] == ".":
                    seafloor_v2[r][(c + 1) % width] = ">"
                    seafloor_v2[r][c] = "."

    seafloor_v3 = deepcopy(seafloor_v2)

    for r, row in enumerate(seafloor_v2):
        for c, location in enumerate(row):
            if location == "v":
                if seafloor_v2[(r + 1) % height][c] == ".":
                    seafloor_v3[(r + 1) % height][c] = "v"
                    seafloor_v3[r][c] = "."

    return seafloor_v3


def part1(lines):
    steps = 0
    seafloor = parse_input(lines)

    while True:
        steps += 1
        next_seafloor = step(seafloor)
        if seafloor == next_seafloor:
            return steps
        seafloor = next_seafloor


def part2(lines):
    return "Part 2 of day 25 is having completed all previous days. ¯\_(ツ)_/¯"


class TestDay25(unittest.TestCase):

    def test_parse_input(self):
        seafloor = parse_input([
            "..........",
            ".>v....v..",
            ".......>..",
            "..........",
        ])
        self.assertEqual(seafloor, [
            list(".........."),
            list(".>v....v.."),
            list(".......>.."),
            list(".........."),
        ])

    def test_step_single_line_east_facing(self):
        seafloor = step([list("...>>>>>...")])
        self.assertEqual(seafloor, [list("...>>>>.>..")])

        seafloor = step(seafloor)
        self.assertEqual(seafloor, [list("...>>>.>.>.")])

    def test_step_two_lines_east_facing_wrap(self):
        seafloor = step([
            list(".>>>..>"),
            list(">>>>..>"),
        ])
        self.assertEqual(seafloor, [
            list(">>>.>.."),
            list(">>>.>.>"),
        ])

    def test_step_east_and_south(self):
        seafloor = step([
            list(".........."),
            list(".>v....v.."),
            list(".......>.."),
            list(".........."),
        ])
        self.assertEqual(seafloor, [
            list(".........."),
            list(".>........"),
            list("..v....v>."),
            list(".........."),
        ])

    def test_part1(self):
        steps = part1([
            "v...>>.vv>",
            ".vv>>.vv..",
            ">>.>v>...v",
            ">>v>>.>.v.",
            "v>v.vv.v..",
            ">.>>..v...",
            ".vv..>.>v.",
            "v.v..>>v.v",
            "....v..v.>",
        ])
        self.assertEqual(steps, 58)


if __name__ == '__main__':
    unittest.main(exit=False)
    read_and_solve(__file__, part1, part2)
