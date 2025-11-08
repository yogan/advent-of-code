import sys
import unittest


def parse():
    names, moves = open(filename).read().strip().split("\n\n")
    return names.split(","), [(m[0], int(m[1])) for m in moves.split(",")]


def part1(names, moves):
    pos = 0
    for move, steps in moves:
        if move == "L":
            pos = max(0, pos - steps)
        else:
            pos = min(len(names) - 1, pos + steps)
    return names[pos]


class Tests(unittest.TestCase):
    def test_part1(self):
        self.assertEqual(part1(["A", "B"], []), "A")
        self.assertEqual(part1(["A", "B"], [("L", 1)]), "A")
        self.assertEqual(part1(["A", "B"], [("R", 1)]), "B")
        self.assertEqual(part1(["A", "B"], [("R", 2)]), "B")
        self.assertEqual(part1(["A", "B"], [("R", 1), ("L", 1)]), "A")


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

    names, moves = parse()
    p1 = part1(names, moves)

    check(1, p1, "Fyrryn" if is_sample else "Braeluth")
