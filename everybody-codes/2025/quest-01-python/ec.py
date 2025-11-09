import sys
import unittest


def parse(filename):
    names, moves = open(filename).read().strip().split("\n\n")
    return names.split(","), [
        (1 if m[0] == "R" else -1, int(m[1])) for m in moves.split(",")
    ]


def part1(names, moves):
    pos = 0
    for dir, steps in moves:
        pos = max(0, min(len(names) - 1, pos + dir * steps))
    return names[pos]


def part2(names, moves):
    pos = 0
    for dir, steps in moves:
        pos = (pos + dir * steps) % len(names)
    return names[pos]


def part3(names, moves):
    for dir, steps in moves:
        idx = dir * steps % len(names)
        names[0], names[idx] = names[idx], names[0]
    return names[0]


class Tests(unittest.TestCase):
    def test_part1(self):
        self.assertEqual(part1(["A", "B"], []), "A")
        self.assertEqual(part1(["A", "B"], [(-1, 1)]), "A")
        self.assertEqual(part1(["A", "B"], [(+1, 1)]), "B")
        self.assertEqual(part1(["A", "B"], [(+1, 2)]), "B")
        self.assertEqual(part1(["A", "B"], [(+1, 1), (-1, 1)]), "A")

    def test_part2(self):
        self.assertEqual(part2(["A", "B"], []), "A")
        self.assertEqual(part2(["A", "B"], [(-1, 1)]), "B")
        self.assertEqual(part2(["A", "B"], [(-1, 2)]), "A")
        self.assertEqual(part2(["A", "B"], [(+1, 1)]), "B")
        self.assertEqual(part2(["A", "B"], [(+1, 2)]), "A")
        self.assertEqual(part2(["A", "B", "C"], [(+1, 1), (-1, 2)]), "C")

    def test_part3(self):
        names = ["Vyrdax", "Drakzyph", "Fyrryn", "Elarzris"]
        moves = [(1, 3), (-1, 2), (1, 3), (-1, 3)]
        self.assertEqual(part3(names.copy(), moves[:0]), "Vyrdax")
        self.assertEqual(part3(names.copy(), moves[:1]), "Elarzris")
        self.assertEqual(part3(names.copy(), moves[:2]), "Fyrryn")
        self.assertEqual(part3(names.copy(), moves[:3]), "Vyrdax")
        self.assertEqual(part3(names.copy(), moves[:4]), "Drakzyph")


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(*parse("sample1.txt")), "Fyrryn")
        failures += check(2, part2(*parse("sample2.txt")), "Elarzris")
        failures += check(3, part3(*parse("sample3.txt")), "Drakzyph")
    else:
        failures += check(1, part1(*parse("input1.txt")), "Braeluth")
        failures += check(2, part2(*parse("input2.txt")), "Thazaelor")
        failures += check(3, part3(*parse("input3.txt")), "Quenthyn")

    exit(failures)


def check(part, actual, expected=None):
    failure = 0

    if expected is None:
        symbol = "❔"
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

    is_sample = "-s" in flags or "--sample" in flags
    run_tests = "-t" in flags or "--test" in flags

    if run_tests:
        sys.argv = sys.argv[:1]  # strip args, unittest.main() doesn't like them
        unittest.main(exit=True)

    main()
