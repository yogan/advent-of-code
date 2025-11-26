import sys
import unittest


def parse(filename):
    return list(map(int, open(filename).read().strip().split(",")))


def parse_line(line):
    return list(map(int, line.split(",")))


def part1(xs, size=90):
    res = 0
    for x in xs:
        res += size // x
    return res


def part2(xs):
    res = 1
    for p in primes(xs):
        res *= p
    return res


def part3(xs, blocks=2025_2025_2025_000):
    ps = primes(xs)
    left, right = 1, blocks
    result = -1

    while left <= right:
        mid = (left + right) // 2
        mid_res = part1(ps, size=mid)

        if mid_res == blocks:
            return mid
        elif mid_res < blocks:
            result = mid
            left = mid + 1
        else:
            right = mid - 1

    return result


def primes(xs):
    res = []
    xss = xs.copy()
    while any(xss):
        p = next(i for i, x in enumerate(xss) if x > 0) + 1
        res.append(p)
        for i in range(p - 1, len(xss), p):
            xss[i] -= 1
    return res


class Tests(unittest.TestCase):
    def test_part1(self):
        self.assertEqual(part1([1, 2, 3, 5, 9]), 193)

    def test_part2(self):
        notes = parse("sample2.txt")
        self.assertEqual(part2(notes), 270)

    def test_part3(self):
        notes = parse("sample2.txt")
        self.assertEqual(part3(notes, blocks=1), 1)
        self.assertEqual(part3(notes, blocks=10), 5)
        self.assertEqual(part3(notes, blocks=100), 47)
        self.assertEqual(part3(notes, blocks=1000), 467)
        self.assertEqual(part3(notes, blocks=10000), 4664)
        self.assertEqual(part3(notes, blocks=100000), 46633)
        self.assertEqual(part3(notes, blocks=1000000), 466322)
        self.assertEqual(part3(notes, blocks=10000000), 4663213)
        self.assertEqual(part3(notes, blocks=100000000), 46632125)
        self.assertEqual(part3(notes, blocks=1000000000), 466321244)
        self.assertEqual(part3(notes, blocks=10000000000), 4663212435)
        self.assertEqual(part3(notes, blocks=100000000000), 46632124353)
        self.assertEqual(part3(notes, blocks=1000000000000), 466321243524)
        self.assertEqual(part3(notes, blocks=10000000000000), 4663212435233)
        self.assertEqual(part3(notes, blocks=100000000000000), 46632124352332)
        self.assertEqual(part3(notes, blocks=202520252025000), 94439495762954)


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(parse("sample1.txt")), 193)
        failures += check(2, part2(parse("sample2.txt")), 270)
        failures += check(3, part3(parse("sample2.txt")), 94439495762954)
    else:
        failures += check(1, part1(parse("input1.txt")), 212)
        failures += check(2, part2(parse("input2.txt")), 104872255488)
        failures += check(3, part3(parse("input3.txt")), 94363690209691)

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
        unittest.main(argv=sys.argv[:1])

    main()
