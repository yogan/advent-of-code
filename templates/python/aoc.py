import sys
import unittest


def part1(boxes):
    return sum([volume(box) for box in boxes])


def volume(box):
    l, w, h = box
    return l * w * h


def parse():
    return [
        [int(num) for num in line.strip().split("x")]
        for line in open(filename).readlines()
    ]


class Tests(unittest.TestCase):
    def test_volume(self):
        self.assertEqual(volume([2, 3, 4]), 24)


def main():
    failures = 0
    failures += check(1, part1(parse()), 9876 if is_sample else None)

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
