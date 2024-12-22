import sys
import unittest
from collections import defaultdict

ROUNDS = 2000


def part1(numbers):
    return sum(simulate(n, ROUNDS) for n in numbers)


def part2(numbers):
    prices = []
    diffs = []

    for n in numbers:
        p, d = prices_and_diffs(n, ROUNDS)
        prices.append(p)
        diffs.append(d)

    window = 4
    bananas = defaultdict(int)

    for ps, ds in zip(prices, diffs):
        seen = set()
        for i in range(len(ps) - window + 1):
            diff_seq = tuple(ds[i : i + window])
            if diff_seq in seen:
                continue
            seen.add(diff_seq)
            bananas[diff_seq] += ps[i + window - 1]

    best_seq = max(bananas, key=lambda k: bananas[k])
    return bananas[best_seq]


def simulate(n, rounds):
    for _ in range(rounds):
        n = evolve(n)
    return n


def evolve(n):
    n ^= n << 6
    n = n % 16777216
    n ^= n >> 5
    n = n % 16777216
    n ^= n << 11
    return n % 16777216


def prices_and_diffs(n, rounds):
    prev = n % 10
    prices = []
    diffs = []

    for _ in range(rounds):
        n = evolve(n)
        d = n % 10
        prices.append(d)
        diffs.append(d - prev)
        prev = d

    return prices, diffs


class Tests(unittest.TestCase):
    def test_simulate(self):
        self.assertEqual(simulate(123, 1), 15887950)
        self.assertEqual(simulate(123, 2), 16495136)
        self.assertEqual(simulate(123, 3), 527345)
        self.assertEqual(simulate(123, 4), 704524)
        self.assertEqual(simulate(123, 5), 1553684)
        self.assertEqual(simulate(123, 6), 12683156)
        self.assertEqual(simulate(123, 7), 11100544)
        self.assertEqual(simulate(123, 8), 12249484)
        self.assertEqual(simulate(123, 9), 7753432)
        self.assertEqual(simulate(123, 10), 5908254)

    def test_prices_and_diffs(self):
        p, d = prices_and_diffs(123, 9)
        self.assertEqual(p, [+0, +6, +5, +4, +4, +6, +4, +4, +2])
        self.assertEqual(d, [-3, +6, -1, -1, +0, +2, -2, +0, -2])


def parse(filename):
    return [int(line.strip()) for line in open(filename).readlines()]


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

    p1 = part1(parse(filename))
    p2 = part2(parse("sample2.txt" if is_sample else filename))

    check(1, p1, 37327623 if is_sample else 17163502021)
    check(2, p2, 23 if is_sample else 1938)
