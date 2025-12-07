import sys
import unittest
from collections import Counter


def travel_multiverses(rows):
    splits = 0
    beams = Counter({c: 1 for c in rows[0]})

    for splitters in rows[1:]:
        collisions = set()
        next_beams = Counter()

        for c, tl in beams.items():
            if c in splitters:
                collisions.add(c)
                next_beams[c - 1] += tl
                next_beams[c + 1] += tl
            else:
                next_beams[c] += tl

        splits += len(collisions)
        beams = next_beams

    return splits, sum(beams.values())


def parse():
    return columns(open(filename).readlines())


def columns(lines):
    return [
        [c for c, ch in enumerate(line.strip()) if ch != "."]
        for line in lines
        if line.replace(".", "")
    ]


class Tests(unittest.TestCase):
    def test_columns(self):
        self.assertEqual(
            columns(
                [
                    ".......S.......",
                    "...............",
                    ".......^.......",
                    "...............",
                    "......^.^......",
                ]
            ),
            [[7], [7], [6, 8]],
        )


def main():
    p1, p2 = travel_multiverses(parse())

    failures = 0
    failures += check(1, p1, 21 if is_sample else 1507)
    failures += check(2, p2, 40 if is_sample else 1537373473728)

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
