import sys
import unittest


def parse():
    return [
        [int(num) for num in line.strip().split()]
        for line in open(filename).readlines()
    ]


def is_safe(levels):
    diffs = [a - b for a, b in zip(levels, levels[1:])]
    return all([1 <= d <= 3 for d in diffs]) or all([-3 <= d <= -1 for d in diffs])


def is_safe_enough(levels):
    return any(is_safe(levels[:i] + levels[i + 1 :]) for i in range(len(levels)))


class Tests(unittest.TestCase):
    def test_is_safe(self):
        self.assertEqual(is_safe([7, 6, 4, 2, 1]), True)
        self.assertEqual(is_safe([1, 2, 7, 8, 9]), False)
        self.assertEqual(is_safe([9, 7, 6, 2, 1]), False)
        self.assertEqual(is_safe([1, 3, 2, 4, 5]), False)
        self.assertEqual(is_safe([8, 6, 4, 4, 1]), False)
        self.assertEqual(is_safe([1, 3, 6, 7, 9]), True)

    def test_is_safe_enough(self):
        self.assertEqual(is_safe_enough([7, 6, 4, 2, 1]), True)
        self.assertEqual(is_safe_enough([1, 2, 7, 8, 9]), False)
        self.assertEqual(is_safe_enough([9, 7, 6, 2, 1]), False)
        self.assertEqual(is_safe_enough([1, 3, 2, 4, 5]), True)
        self.assertEqual(is_safe_enough([8, 6, 4, 4, 1]), True)
        self.assertEqual(is_safe_enough([1, 3, 6, 7, 9]), True)


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]

    filename = "sample.txt" if "-s" in flags or "--sample" in flags else "input.txt"
    filename = args[0] if args else filename
    is_sample = filename.startswith("sample")
    run_tests = "-t" in flags or "--test" in flags

    if run_tests:
        unittest.main(argv=sys.argv[:1])

    def check(part, actual, expected=None):
        print(f"Part {part}{' (sample)' if is_sample else ''}: {actual} ", end="")
        if expected is None:
            print("❔")
        else:
            if actual != expected:
                print(f"≠ {expected} ❌")
                exit(1)
            print("✅")

    reports = parse()
    part1 = sum(is_safe(report) for report in reports)
    part2 = sum(is_safe_enough(report) for report in reports)

    check(1, part1, 2 if is_sample else 252)
    check(2, part2, 4 if is_sample else 324)
