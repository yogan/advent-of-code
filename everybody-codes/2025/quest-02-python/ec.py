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


def engravings(a, step):
    ax, ay = a
    dim = 1001
    y_coords = list(range(ay, ay + dim, step))
    x_coords = list(range(ax, ax + dim, step))
    grid = [[0 for _ in range(len(x_coords))] for _ in range(len(y_coords))]

    if visualize:
        from tqdm import tqdm

        y_iter = tqdm(enumerate(y_coords), desc="Filling grid", total=len(y_coords))
    else:
        y_iter = enumerate(y_coords)

    for y_idx, y in y_iter:
        for x_idx, x in enumerate(x_coords):
            grid[y_idx][x_idx] = engrave((x, y))

    return grid


def count(grid):
    return sum(1 for row in grid for val in row if val == 0)


def engrave(coord):
    val = (0, 0)
    limit = 1_000_000

    for i in range(100):
        val = mult(val, val)
        val = div(val, (100_000, 100_000))
        val = add(val, coord)
        x, y = val
        if x > limit or x < -limit or y > limit or y < -limit:
            return i + 1

    return 0


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
    return int(ax / bx), int(ay / by)


def to_str(a):
    ax, ay = a
    return f"[{ax},{ay}]"


def to_image(grid, filename):
    from math import pi, sin

    from PIL import Image
    from tqdm import tqdm

    height = len(grid)
    width = len(grid[0])
    img = Image.new("RGB", (width, height))

    def color(iterations):
        if iterations == 0:
            return 0, 0, 0

        t = iterations / 100.0
        r = int(128 + 127 * sin(2 * pi * t))
        g = int(128 + 127 * sin(2 * pi * t + 2 * pi / 3))
        b = int(128 + 127 * sin(2 * pi * t + 4 * pi / 3))

        return r, g, b

    for y in tqdm(range(height), desc="Creating image"):
        for x in range(width):
            img.putpixel((x, y), color(grid[y][x]))

    img.save(filename)
    print(f"Image saved to {filename}")


class Tests(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add((-2, 5), (10, -1)), (8, 4))

    def test_mult(self):
        self.assertEqual(mult((-2, 5), (10, -1)), (-15, 52))

    def test_div(self):
        self.assertEqual(div((-10, -12), (2, 2)), (-5, -6))

    def test_engrave(self):
        self.assertEqual(engrave((35630, -64880)), 0)
        self.assertEqual(engrave((35630, -64870)), 0)
        self.assertEqual(engrave((35640, -64860)), 0)
        self.assertEqual(engrave((36230, -64270)), 0)
        self.assertEqual(engrave((36250, -64270)), 0)

        self.assertGreater(engrave((35460, -64910)), 0)
        self.assertGreater(engrave((35470, -64910)), 0)
        self.assertGreater(engrave((35480, -64910)), 0)
        self.assertGreater(engrave((35680, -64850)), 0)
        self.assertGreater(engrave((35630, -64830)), 0)


def main():
    failures = 0

    if is_sample:
        failures += check(1, to_str(part1(parse("sample1.txt"))), "[357,862]")
        failures += check(2, count(engravings(parse("sample2.txt"), 10)), 4076)
        failures += check(3, count(engravings(parse("sample2.txt"), 1)), 406954)
    else:
        failures += check(1, to_str(part1(parse("input1.txt"))), "[483530,983550]")
        failures += check(2, count(engravings(parse("input2.txt"), 10)), 632)
        failures += check(3, count(engravings(parse("input2.txt"), 1)), 60697)

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
    visualize = "-v" in flags or "--visualize" in flags

    if run_tests:
        sys.argv = sys.argv[:1]  # strip args, unittest.main() doesn't like them
        unittest.main(exit=True)
    elif visualize:
        to_image(engravings(parse("sample2.txt"), 1), "sample.png")
        to_image(engravings(parse("input2.txt"), 1), "input.png")
    else:
        main()
