import sys
import unittest


def part1(positions):
    best = 0
    for r1, c1 in positions:
        for r2, c2 in positions:
            area = abs(r2 - r1 + 1) * abs(c2 - c1 + 1)
            best = max(best, area)
    return best


def parse():
    return [
        tuple(map(int, line.strip().split(","))) for line in open(filename).readlines()
    ]


class Tests(unittest.TestCase):
    def test_part1(self):
        self.assertEqual(
            part1(
                [
                    (7, 1),
                    (11, 1),
                    (11, 7),
                    (9, 7),
                    (9, 5),
                    (2, 5),
                    (2, 3),
                    (7, 3),
                ]
            ),
            50,
        )


def main():
    failures = 0
    failures += check(1, part1(parse()), 50 if is_sample else 4746238001)

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

    print(f"{symbol} Part {part}{' (sample)' if is_sample else ''}: {result}")
    return failure


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]

    is_sample = "-s" in flags or "--sample" in flags
    run_tests = "-t" in flags or "--test" in flags

    filename = "sample.txt" if is_sample else "input.txt"
    filename = args[0] if args else filename

    if run_tests:
        unittest.main(argv=sys.argv[:1])

    main()
