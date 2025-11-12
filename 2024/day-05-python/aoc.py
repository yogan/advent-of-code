import sys
import unittest


def parse(filename):
    rules, updates = open(filename).read().strip().split("\n\n")
    return (
        [line.split("|") for line in rules.split("\n")],
        [line.split(",") for line in updates.split("\n")],
    )


def is_ok(update, rules):
    for left, right in rules:
        if left not in update or right not in update:
            continue
        if update.index(left) > update.index(right):
            return False

    return True


def fix(update, rules):
    pages = [update[0]]

    for page in update[1:]:
        for i in range(len(pages) + 1):
            candidate = pages[:i] + [page] + pages[i:]
            if is_ok(candidate, rules):
                pages = candidate
                break

    return pages


def sum_updates(updates):
    res = 0
    for update in updates:
        middle = int(update[len(update) // 2])
        res += middle
    return res


def part1(rules, updates):
    return sum_updates([update for update in updates if is_ok(update, rules)])


def part2(rules, updates):
    incorrect = [update for update in updates if not is_ok(update, rules)]
    fixed = [fix(update, rules) for update in incorrect]

    return sum_updates(fixed)


class Tests(unittest.TestCase):
    rules, updates = parse("sample.txt")

    def test_is_ok(self):
        self.assertEqual(is_ok(self.updates[0], self.rules), True)
        self.assertEqual(is_ok(self.updates[1], self.rules), True)
        self.assertEqual(is_ok(self.updates[2], self.rules), True)
        self.assertEqual(is_ok(self.updates[3], self.rules), False)
        self.assertEqual(is_ok(self.updates[4], self.rules), False)
        self.assertEqual(is_ok(self.updates[5], self.rules), False)

    def test_fix(self):
        self.assertEqual(
            fix(["75", "97", "47", "61", "53"], self.rules),
            ["97", "75", "47", "61", "53"],
        )


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

    rules, updates = parse(filename)
    p1 = part1(rules, updates)
    p2 = part2(rules, updates)

    check(1, p1, 143 if is_sample else 7074)
    check(2, p2, 123 if is_sample else 4828)
