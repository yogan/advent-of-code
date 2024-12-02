import sys
import unittest


def parse():
    return [
        [int(num) for num in line.strip().split()]
        for line in open(filename).readlines()
    ]


def is_safe(levels):
    diffs = [b - a for a, b in zip(levels, levels[1:])]
    signs = [d > 0 for d in diffs]

    all_increasing = all(signs)
    all_decreasing = all(not i for i in signs)
    diffs_in_range = all([1 <= abs(d) <= 3 for d in diffs])

    return (all_increasing or all_decreasing) and diffs_in_range


def is_safe_enough(levels):
    if is_safe(levels):
        return True

    for i in range(0, len(levels)):
        levels_without_i = levels[:i] + levels[i + 1 :]
        if is_safe(levels_without_i):
            return True

    return False


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
    sys.argv = sys.argv[:1]  # strip args, unittest.main() doesn't like them

    filename = "sample.txt" if "-s" in flags or "--sample" in flags else "input.txt"
    filename = args[0] if args else filename
    is_sample = filename.startswith("sample")
    run_tests = "-t" in flags or "--test" in flags

    if run_tests:
        unittest.main(exit=True)

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
    part1 = sum(1 for report in reports if is_safe(report))
    part2 = sum(1 for report in reports if is_safe_enough(report))

    check(1, part1, 2 if is_sample else 252)
    check(2, part2, 4 if is_sample else 324)
