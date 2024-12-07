import sys
import unittest
from itertools import product


def parse():
    return [parse_line(line) for line in open(filename).readlines()]


def parse_line(line):
    target, rest = line.split(": ")
    return int(target), list(map(int, rest.split(" ")))


def possible(target, rest, concat=False):
    blanks = len(rest) - 1

    for ops in op_combinations(blanks, concat=concat):
        result = rest[0]

        for op, num in zip(ops, rest[1:]):
            if op == "+":
                result += num
            elif op == "*":
                result *= num
            elif op == "|":
                result = int(str(result) + str(num))

        if result == target:
            return True

    return False


def op_combinations(blanks, concat=False):
    return product("+*|" if concat else "+*", repeat=blanks)


def part1(lines):
    return sum(target for target, rest in lines if possible(int(target), rest))


def part2(lines):
    return sum(
        target for target, rest in lines if possible(int(target), rest, concat=True)
    )


class Tests(unittest.TestCase):
    def test_parse_line(self):
        self.assertEqual(parse_line("21037: 9 7 18 13"), (21037, [9, 7, 18, 13]))

    def test_op_combinations(self):
        self.assertEqual(set(op_combinations(1)), {("+",), ("*",)})
        self.assertEqual(
            set(op_combinations(2)), {("+", "+"), ("+", "*"), ("*", "+"), ("*", "*")}
        )
        self.assertEqual(
            set(op_combinations(3)),
            {
                ("+", "+", "+"),
                ("+", "+", "*"),
                ("+", "*", "+"),
                ("+", "*", "*"),
                ("*", "+", "+"),
                ("*", "+", "*"),
                ("*", "*", "+"),
                ("*", "*", "*"),
            },
        )

    def test_op_combincations_concat(self):
        concat = True
        self.assertEqual(
            set(op_combinations(1, concat=concat)), {("+",), ("*",), ("|",)}
        )
        self.assertEqual(
            set(op_combinations(2, concat=concat)),
            {
                ("+", "+"),
                ("+", "*"),
                ("+", "|"),
                ("*", "+"),
                ("*", "*"),
                ("*", "|"),
                ("|", "+"),
                ("|", "*"),
                ("|", "|"),
            },
        )

    def test_possible(self):
        self.assertTrue(possible(190, [10, 19]))
        self.assertTrue(possible(3267, [81, 40, 27]))
        self.assertFalse(possible(83, [17, 5]))
        self.assertFalse(possible(156, [15, 6]))
        self.assertFalse(possible(7290, [6, 8, 6, 15]))
        self.assertFalse(possible(161011, [16, 10, 13]))
        self.assertFalse(possible(192, [17, 8, 14]))
        self.assertFalse(possible(21037, [9, 7, 18, 13]))
        self.assertTrue(possible(292, [11, 6, 16, 20]))

    def test_possible_with_concat(self):
        self.assertTrue(possible(190, [10, 19], concat=True))
        self.assertTrue(possible(3267, [81, 40, 27], concat=True))
        self.assertFalse(possible(83, [17, 5], concat=True))
        self.assertTrue(possible(156, [15, 6], concat=True))
        self.assertTrue(possible(7290, [6, 8, 6, 15], concat=True))
        self.assertFalse(possible(161011, [16, 10, 13], concat=True))
        self.assertTrue(possible(192, [17, 8, 14], concat=True))
        self.assertFalse(possible(21037, [9, 7, 18, 13], concat=True))
        self.assertTrue(possible(292, [11, 6, 16, 20], concat=True))


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

    lines = parse()
    p1 = part1(lines)
    p2 = part2(lines)

    check(1, p1, 3749 if is_sample else 663613490587)
    check(2, p2, 11387 if is_sample else 110365987435001)
