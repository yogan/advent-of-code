import sys
import unittest


def parse(filename):
    parts = open(filename).read().split(",")
    return int(parts[0][3:]), int(parts[1][:-2])


def part1(a):
    r = 0, 0
    for _ in range(3):
        r = mult(r, r)
        r = div(r, (10, 10))
        r = add(r, a)
    return r


def add(a, b):
    ax, ay = a
    bx, by = b
    return ax + bx, ay + by


def mult(a, b):
    ax, ay = a
    bx, by = b
    return ax * bx - ay * by, ax * by + ay * bx


def div(a, b):
    ax, ay = a
    bx, by = b
    return ax // bx, ay // by


def to_str(a):
    ax, ay = a
    return f"[{ax},{ay}]"


class Tests(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add((-2, 5), (10, -1)), (8, 4))

    def test_mult(self):
        self.assertEqual(mult((-2, 5), (10, -1)), (-15, 52))

    def test_div(self):
        self.assertEqual(div((-10, -12), (2, 2)), (-5, -6))


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]
    sys.argv = sys.argv[:1]  # strip args, unittest.main() doesn't like them

    is_sample = "-s" in flags or "--sample" in flags
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

    if is_sample:
        check(1, to_str(part1(parse("sample1.txt"))), "[357,862]")
    else:
        check(1, to_str(part1(parse("input1.txt"))), "[483530,983550]")
