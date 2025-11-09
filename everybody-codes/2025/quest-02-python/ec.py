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
    grid = [[False for _ in range(len(x_coords))] for _ in range(len(y_coords))]

    for y_idx, y in enumerate(y_coords):
        for x_idx, x in enumerate(x_coords):
            grid[y_idx][x_idx] = engrave((x, y))

    return grid


def count(grid):
    return sum(sum(row) for row in grid)


def to_image(grid, filename):
    from PIL import Image

    height = len(grid)
    width = len(grid[0]) if height > 0 else 0
    img = Image.new("RGB", (width, height))

    for y in range(height):
        for x in range(width):
            if grid[y][x]:
                img.putpixel((x, y), (255, 255, 255))

    img.save(filename)
    print(f"Image saved to {filename}")


def engrave(coord):
    val = (0, 0)
    limit = 1_000_000

    for _ in range(100):
        val = mult(val, val)
        val = div(val, (100_000, 100_000))
        val = add(val, coord)
        x, y = val
        if x > limit or x < -limit or y > limit or y < -limit:
            return False

    return True


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


class Tests(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add((-2, 5), (10, -1)), (8, 4))

    def test_mult(self):
        self.assertEqual(mult((-2, 5), (10, -1)), (-15, 52))

    def test_div(self):
        self.assertEqual(div((-10, -12), (2, 2)), (-5, -6))

    def test_engrave(self):
        self.assertTrue(engrave((35630, -64880)))
        self.assertTrue(engrave((35630, -64870)))
        self.assertTrue(engrave((35640, -64860)))
        self.assertTrue(engrave((36230, -64270)))
        self.assertTrue(engrave((36250, -64270)))

        self.assertFalse(engrave((35460, -64910)))
        self.assertFalse(engrave((35470, -64910)))
        self.assertFalse(engrave((35480, -64910)))
        self.assertFalse(engrave((35680, -64850)))
        self.assertFalse(engrave((35630, -64830)))


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]
    sys.argv = sys.argv[:1]  # strip args, unittest.main() doesn't like them

    is_sample = "-s" in flags or "--sample" in flags
    run_tests = "-t" in flags or "--test" in flags
    visualize = "-v" in flags or "--visualize" in flags

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
        check(2, count(engravings(parse("sample2.txt"), 10)), 4076)
        grid3 = engravings(parse("sample2.txt"), 1)
        check(3, count(grid3), 406954)
        if visualize:
            to_image(grid3, "sample.png")
    else:
        check(1, to_str(part1(parse("input1.txt"))), "[483530,983550]")
        check(2, count(engravings(parse("input2.txt"), 10)), 632)
        grid3 = engravings(parse("input2.txt"), 1)
        check(3, count(grid3), 60697)
        if visualize:
            to_image(grid3, "input.png")
