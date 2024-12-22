import sys
import unittest


def part1(numbers):
    return sum(simulate(n, 2000) for n in numbers)


def simulate(n, rounds):
    for _ in range(rounds):
        n ^= n << 6
        n = n % 16777216
        n ^= n >> 5
        n = n % 16777216
        n ^= n << 11
        n = n % 16777216
    return n


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

    numbers = [int(line.strip()) for line in open(filename).readlines()]
    p1 = part1(numbers)
    p2 = None

    check(1, p1, 37327623 if is_sample else 17163502021)
    check(2, p2)
