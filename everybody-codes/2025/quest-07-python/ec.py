import sys
import unittest


def parse(filename):
    names, _, *orders = open(filename).readlines()
    rules = {}
    for order in orders:
        l, r = order.strip().split(" > ")
        rules[l] = r.split(",")
    return names.strip().split(","), rules


def part1(names, rules):
    for name in names:
        if valid(name, rules):
            return name


def valid(name, rules):
    for i in range(len(name) - 1):
        # if name[i] not in rules:
        #     return False
        if name[i + 1] not in rules[name[i]]:
            return False
    return True


class Tests(unittest.TestCase):
    def test_part1(self):
        names = ["Oronris", "Urakris", "Oroneth", "Uraketh"]
        rules = {
            "r": ["a", "i", "o"],
            "i": ["p", "w"],
            "n": ["e", "r"],
            "o": ["n", "m"],
            "k": ["f", "r"],
            "a": ["k"],
            "U": ["r"],
            "e": ["t"],
            "O": ["r"],
            "t": ["h"],
        }
        self.assertEqual(part1(names, rules), "Oroneth")


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(*parse("sample1.txt")), "Oroneth")
    else:
        failures += check(1, part1(*parse("input1.txt")), "Nymirath")

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

    is_sample = "-s" in flags or "--sample" in flags
    run_tests = "-t" in flags or "--test" in flags

    if run_tests:
        sys.argv = sys.argv[:1]  # strip args, unittest.main() doesn't like them
        unittest.main(exit=True)

    main()
