import sys
import unittest


def parse(filename):
    robots = []
    for line in open(filename).readlines():
        p, v = line.strip().split()
        px, py = p[2:].split(",")
        vx, vy = v[2:].split(",")
        robots.append(
            {
                "p": (int(px), int(py)),
                "v": (int(vx), int(vy)),
            }
        )
    return robots


def simulate(robots, seconds, max_x, max_y):
    positions = []
    for robot in robots:
        px, py = robot["p"]
        vx, vy = robot["v"]
        xx, yy = px + vx * seconds, py + vy * seconds
        positions.append((xx % max_x, yy % max_y))
    return positions


def count_quadrants(positions, max_x, max_y):
    x_mid = max_x // 2
    y_mid = max_y // 2
    top_left = top_right = bottom_left = bottom_right = 0
    for x, y in positions:
        if x < x_mid and y < y_mid:
            top_left += 1
        elif x > x_mid and y < y_mid:
            top_right += 1
        elif x < x_mid and y > y_mid:
            bottom_left += 1
        elif x > x_mid and y > y_mid:
            bottom_right += 1
    return (top_left, top_right, bottom_left, bottom_right)


def has_long_horizontal_line(positions, max_x, max_y):
    for y in range(max_y):
        longest = 0
        count = 0
        for x in range(1, max_x):
            if (x, y) in positions:
                count += 1
            else:
                if count > longest:
                    longest = count
                count = 0
        if count > longest:
            longest = count
        if longest >= 10:
            return True
    return False


def part1(robots, max_x, max_y):
    positions = simulate(robots, 100, max_x, max_y)
    tl, tr, bl, br = count_quadrants(positions, max_x, max_y)
    return tl * tr * bl * br


def part2(robots, max_x, max_y):
    for s in range(10100):
        positions = simulate(robots, s, max_x, max_y)
        if has_long_horizontal_line(positions, max_x, max_y):
            # Comment in to see the Christmas tree:
            # for y in range(max_y):
            #     for x in range(max_x):
            #         if (x, y) in positions:
            #             print("#", end="")
            #         else:
            #             print(".", end="")
            #     print()
            return s


class Tests(unittest.TestCase):
    def test_simulate(self):
        max_x, max_y = 11, 7
        self.assertEqual(
            simulate([{"p": (2, 4), "v": (2, -3)}], 1, max_x, max_y), [(4, 1)]
        )
        self.assertEqual(
            simulate([{"p": (2, 4), "v": (2, -3)}], 2, max_x, max_y), [(6, 5)]
        )
        self.assertEqual(
            simulate([{"p": (2, 4), "v": (2, -3)}], 3, max_x, max_y), [(8, 2)]
        )
        self.assertEqual(
            simulate([{"p": (2, 4), "v": (2, -3)}], 4, max_x, max_y), [(10, 6)]
        )
        self.assertEqual(
            simulate([{"p": (2, 4), "v": (2, -3)}], 5, max_x, max_y), [(1, 3)]
        )

    def test_count_quadrants(self):
        max_x, max_y = 11, 7
        positions = [
            (3, 5),
            (5, 4),
            (9, 0),
            (4, 5),
            (1, 6),
            (1, 3),
            (6, 0),
            (2, 3),
            (0, 2),
            (6, 0),
            (4, 5),
            (6, 6),
        ]
        self.assertEqual(count_quadrants(positions, max_x, max_y), (1, 3, 4, 1))


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

    robots = parse(filename)
    max_x = 11 if is_sample else 101
    max_y = +7 if is_sample else 103
    p1 = part1(robots, max_x, max_y)
    p2 = "n/a" if is_sample else part2(robots, max_x, max_y)

    check(1, p1, 12 if is_sample else 222208000)
    check(2, p2, "n/a" if is_sample else 7623)
