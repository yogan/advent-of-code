import unittest
from input import read_and_solve


def parse_input(lines):
    index = lines.index("")
    dot_lines, fold_lines = (lines[:index], lines[index+1:])

    dot_pairs = [s.split(",") for s in dot_lines]
    dots = {(int(x), int(y)) for x, y in dot_pairs}

    fold_pairs = [s.split("=") for s in fold_lines]
    folds = [(axis[-1], int(value)) for axis, value in fold_pairs]

    return (dots, folds)


def fold_left(dots, fold_x):
    new_dots     = {(x, y) for x, y in dots if x <  fold_x}
    dots_to_fold = {(x, y) for x, y in dots if x >  fold_x}
    assert     not {(x, y) for x, y in dots if x == fold_x}, "no dots on fold"

    for x, y in dots_to_fold:
        new_dots.add((abs(x - 2 * fold_x), y))

    return new_dots


def fold_up(dots, fold_y):
    new_dots     = {(x, y) for x, y in dots if y <  fold_y}
    dots_to_fold = {(x, y) for x, y in dots if y >  fold_y}
    assert     not {(x, y) for x, y in dots if y == fold_y}, "no dots on fold"

    for x, y in dots_to_fold:
        new_dots.add((x, abs(y - 2 * fold_y)))

    return new_dots


def dots_to_ascii_art(dots):
    width  = max([x for x, _ in dots]) + 1
    height = max([y for _, y in dots]) + 1

    lines = [["."] * width for _ in range(height)]

    for x, y in dots:
        lines[y][x] = "#"

    return ["".join(line) for line in lines]


def part1(lines):
    dots, folds = parse_input(lines)
    axis, coord = folds[0]
    new_dots = fold_left(dots, coord) if axis == "x" else fold_up(dots, coord)
    return len(new_dots)


def part2(lines):
    dots, folds = parse_input(lines)

    for axis, coord in folds:
        dots = fold_left(dots, coord) if axis == "x" else fold_up(dots, coord)

    print()
    for line in dots_to_ascii_art(dots):
        print(line)
    print()

    return "↑ Read this, puny human! ↑"


class TestDay13(unittest.TestCase):

    sample = [
        "6,10",
        "0,14",
        "9,10",
        "0,3",
        "10,4",
        "4,11",
        "6,0",
        "6,12",
        "4,1",
        "0,13",
        "10,12",
        "3,4",
        "3,0",
        "8,4",
        "1,10",
        "2,14",
        "8,10",
        "9,0",
        "",
        "fold along y=7",
        "fold along x=5",
    ]

    def test_parse_input(self):
        (dots, folds) = parse_input(self.sample)
        self.assertEqual(dots, {
            (6, 10),
            (0, 14),
            (9, 10),
            (0, 3),
            (10, 4),
            (4, 11),
            (6, 0),
            (6, 12),
            (4, 1),
            (0, 13),
            (10, 12),
            (3, 4),
            (3, 0),
            (8, 4),
            (1, 10),
            (2, 14),
            (8, 10),
            (9, 0),
        })
        self.assertEqual(folds, [("y", 7), ("x", 5)])

    def test_fold_up(self):
        dots = {  # result of parsing the sample, but in a nice order:
            (0, 3), (0, 13), (0, 14),
            (1, 10),
            (2, 14),
            (3, 0), (3, 4),
            (4, 1), (4, 11),
            (6, 0), (6, 10), (6, 12),
            (8, 4), (8, 10),
            (9, 0), (9, 10),
            (10, 4), (10, 12),
        }

        new_dots = fold_up(dots, 7)

        self.assertEqual(new_dots, {  # carefully recreated from ASCII result:
            (0, 0), (0, 1), (0, 3),
            (1, 4),
            (2, 0),
            (3, 0), (3, 4),
            (4, 1), (4, 3),
            (6, 0), (6, 2), (6, 4),
            (8, 4),
            (9, 0), (9, 4),
            (10, 2), (10, 4),
        })

    def test_fold_left(self):
        dots = {  # result of fold_up(…, 7) from previous test / sample:
            (0, 0), (0, 1), (0, 3),
            (1, 4),
            (2, 0),
            (3, 0), (3, 4),
            (4, 1), (4, 3),
            (6, 0), (6, 2), (6, 4),
            (8, 4),
            (9, 0), (9, 4),
            (10, 2), (10, 4),
        }

        new_dots = fold_left(dots, 5)

        self.assertEqual(new_dots, {  # carefully recreated from ASCII result:
            (0, 0), (0, 1), (0, 2), (0, 3), (0, 4),
            (1, 0),                         (1, 4),
            (2, 0),                         (2, 4),
            (3, 0),                         (3, 4),
            (4, 0), (4, 1), (4, 2), (4, 3), (4, 4),
        })

    def test_dots_to_ascii_art(self):
        dots = {
            (0, 0), (0, 1), (0, 2), (0, 3), (0, 4),
            (1, 0),                         (1, 4),
            (2, 0),                         (2, 4),
            (3, 0),                         (3, 4),
            (4, 0), (4, 1), (4, 2), (4, 3), (4, 4),
        }

        ascii_art = dots_to_ascii_art(dots)

        self.assertEqual(ascii_art, [
            "#####",
            "#...#",
            "#...#",
            "#...#",
            "#####",
        ])

    def test_part_1_sample(self):
        self.assertEqual(part1(self.sample), 17)

    def test_part_2_sample(self):
        self.assertEqual(part2(self.sample), "↑ Read this, puny human! ↑")


if __name__ == '__main__':
    unittest.main(exit=False)
    read_and_solve(__file__, part1, part2)
