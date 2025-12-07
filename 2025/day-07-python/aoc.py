import sys
import unittest


def part1(rows):
    splits, beams = 0, rows[0]

    for splitters in rows[1:]:
        collisions = beams & splitters
        splits += len(collisions)

        new = {n for c in collisions for n in (c - 1, c + 1)}
        beams = beams - collisions | new

    return splits


def parse():
    return columns(open(filename).readlines())


def columns(lines):
    return [{c for c, ch in enumerate(l.strip()) if ch != "."} for l in lines]


class Tests(unittest.TestCase):
    def test_columns(self):
        self.assertEqual(
            columns(
                [
                    # 1234567
                    ".......S.......",
                    "...............",
                    ".......^.......",
                    "...............",
                    "......^.^......",
                    #      6 8
                ]
            ),
            [{7}, set(), {7}, set(), {6, 8}],
        )


def main():
    failures = 0
    failures += check(1, part1(parse()), 21 if is_sample else 1507)

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
