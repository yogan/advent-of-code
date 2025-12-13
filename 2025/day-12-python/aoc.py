import sys
import unittest


def part1(regions):
    return sum(
        area >= sum(shape_area(s, q) for s, q in enumerate(quantities))
        for area, quantities in regions
    )


# "Good Enough Packing"™
#
#   0: ##.   .11             5x4 for 2
#      ###   11222
#      #.#   11122
#              22
#
#   1: ###   111             4x4 for 2
#      #.#   1212
#      #.#   1212
#             222
#
#   2: #..   1222            4x3 for 2
#      ##.   1122
#      ###   1112
#
#   3: ###   111 333 555    12x4 for 6
#      .#.   .122234445666   6x2 for 3
#      ###   111233345556
#              222 444 666
#
#   4: ..#   1223344.        4x3 for 2
#      .##   11223344
#      ##.   .112.3.4
#
#   5: ###   11122           5x3 for 2
#      ###   11122
#      ..#   1.222
#
def shape_area(shape, quantity):
    if shape == 0:
        return 5 * 4 * quantity / 2
    if shape == 1:
        return 4 * 4 * quantity / 2
    if shape == 2:
        return 4 * 3 * quantity / 2
    if shape == 3:
        return 6 * 2 * quantity / 3
    if shape == 4:
        return 4 * 3 * quantity / 2
    if shape == 5:
        return 5 * 3 * quantity / 2

    raise ValueError(f"invalid shape {shape}")


def parse():
    *_, regions = open(filename).read().split("\n\n")
    return [parse_region(r) for r in regions.strip().split("\n")]


def parse_region(region):
    size, quantities = region.split(": ")
    return eval(size.replace("x", "*")), tuple(map(int, quantities.split()))


class Tests(unittest.TestCase):
    def test_parse_region(self):
        self.assertEqual(
            parse_region("35x49: 30 30 32 26 30 28"),
            (1715, (30, 30, 32, 26, 30, 28)),
        )


def main():
    failures = check(1, part1(parse()), 550)
    exit(failures)


def check(part, actual, expected=None):
    failure = 0

    if expected is None:
        symbol = "🤔"
        result = f"{actual}"
    elif actual == expected:
        symbol = "✅"
        result = f"{actual}"
    else:
        symbol = "❌"
        result = f"{actual} ≠ {expected}"
        failure = 1

    print(f"{symbol} Part {part}: {result}")
    return failure


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]

    run_tests = "-t" in flags or "--test" in flags
    filename = args[0] if args else "input.txt"

    if run_tests:
        unittest.main(argv=sys.argv[:1])

    main()
