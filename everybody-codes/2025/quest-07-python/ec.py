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


def part2(names, rules):
    total = 0
    for i, name in enumerate(names, 1):
        if valid(name, rules):
            total += i
    return total


def part3(names, rules):
    res = []
    for name in names:
        if valid(name, rules):
            res += possible_suffixes(name, rules)
    return len(set(res))  # set to remove duplicates


def valid(name, rules):
    for i in range(len(name) - 1):
        if name[i + 1] not in rules[name[i]]:
            return False
    return True


def possible_suffixes(name, rules):
    min, max = 7 - len(name), 11 - len(name)
    res = []
    for length in range(min, max + 1):
        res += combinations(name, rules, length)
    return res


def combinations(prefix, rules, length):
    letter = prefix[-1]
    next_letters = rules.get(letter, [])

    if length == 1:
        return [prefix + next for next in next_letters]

    res = []
    for next in next_letters:
        res += combinations(prefix + next, rules, length - 1)
    return res


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

    def test_combinations(self):
        name = "Xaryt"
        rules = {
            "X": ["a", "o"],
            "a": ["r", "t"],
            "r": ["y", "e", "a"],
            "h": ["a", "e", "v"],
            "t": ["h"],
            "v": ["e"],
            "y": ["p", "t"],
        }
        self.assertEqual(
            combinations(name, rules, 2),
            [
                "Xarytha",
                "Xarythe",
                "Xarythv",
            ],
        )
        self.assertEqual(
            combinations(name, rules, 3),
            [
                "Xarythar",
                "Xarythat",
                "Xarythve",
            ],
        )
        self.assertEqual(
            combinations(name, rules, 4),
            [
                "Xarythary",
                "Xarythare",
                "Xarythara",
                "Xarythath",
            ],
        )
        self.assertEqual(
            combinations(name, rules, 5),
            [
                "Xarytharyp",
                "Xarytharyt",
                "Xarytharar",
                "Xarytharat",
                "Xarythatha",
                "Xarythathe",
                "Xarythathv",
            ],
        )
        self.assertEqual(
            combinations(name, rules, 6),
            [
                "Xarytharyth",
                "Xarytharary",
                "Xarytharare",
                "Xarytharara",
                "Xarytharath",
                "Xarythathar",
                "Xarythathat",
                "Xarythathve",
            ],
        )

    def test_possible_suffixes(self):
        rules = {
            "X": ["a", "o"],
            "a": ["r", "t"],
            "r": ["y", "e", "a"],
            "h": ["a", "e", "v"],
            "t": ["h"],
            "v": ["e"],
            "y": ["p", "t"],
        }
        self.assertEqual(
            possible_suffixes("Xaryt", rules),
            [
                "Xarytha",
                "Xarythe",
                "Xarythv",
                "Xarythar",
                "Xarythat",
                "Xarythve",
                "Xarythary",
                "Xarythare",
                "Xarythara",
                "Xarythath",
                "Xarytharyp",
                "Xarytharyt",
                "Xarytharar",
                "Xarytharat",
                "Xarythatha",
                "Xarythathe",
                "Xarythathv",
                "Xarytharyth",
                "Xarytharary",
                "Xarytharare",
                "Xarytharara",
                "Xarytharath",
                "Xarythathar",
                "Xarythathat",
                "Xarythathve",
            ],
        )


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(*parse("sample1.txt")), "Oroneth")
        failures += check(2, part2(*parse("sample2.txt")), 23)
        failures += check("3a", part3(*parse("sample3a.txt")), 25)
        failures += check("3b", part3(*parse("sample3b.txt")), 1_154)
    else:
        failures += check(1, part1(*parse("input1.txt")), "Nymirath")
        failures += check(2, part2(*parse("input2.txt")), 3_289)
        failures += check(3, part3(*parse("input3.txt")), 3_170_031)

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
