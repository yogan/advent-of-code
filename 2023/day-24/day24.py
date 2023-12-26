import sys, unittest
from z3 import *

if len(sys.argv) != 2:
    print("Missing input file.")
    exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they confuse the unittest module
is_sample = filename == "sample.txt"

class Hailstone:
    def __init__(self, p_str, v_str):
        self.position = self.parse(p_str)
        self.velocity = self.parse(v_str)

    def parse(self, string):
        return tuple(map(int, string.split(", ")))

    def __repr__(self):
        return f"{self.position}@{self.velocity}"

    def get_2d_line_coefficients(self):
        px, py, _ = self.position
        vx, vy, _ = self.velocity
        return vy, -vx, vx * py - vy * px

def parse():
    return [Hailstone(*line.strip().split(" @ "))
            for line in open(filename).readlines()]

def intersect2d(stone1, stone2):
    a1, b1, c1 = stone1.get_2d_line_coefficients()
    a2, b2, c2 = stone2.get_2d_line_coefficients()

    # Using homogeneous coordinates, see:
    # https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Using_homogeneous_coordinates
    ap, bp, cp = b1*c2 - b2*c1, a2*c1 - a1*c2, a1*b2 - a2*b1

    if cp == 0:
        return None  # lines are parallel

    # map back to 2d space
    return ap / cp, bp / cp

def is_in_test_area(point):
    lower_limit =  7 if is_sample else 200_000_000_000_000
    upper_limit = 27 if is_sample else 400_000_000_000_000

    return  lower_limit <= point[0] <= upper_limit and \
            lower_limit <= point[1] <= upper_limit

def is_in_past(point, stone):
    # We only check the x coordinate, which is fine, because we don't have any
    # stones with 0 for any of the velocity vector components.
    return point[0] < stone.position[0] if stone.velocity[0] > 0 else \
           point[0] > stone.position[0]

def count_intersections(hailstones):
    intersections = 0

    for i in range(len(hailstones)):
        for j in range(i + 1, len(hailstones)):
            point = intersect2d(hailstones[i], hailstones[j])
            if point is not None and is_in_test_area(point)  \
                    and not is_in_past(point, hailstones[i]) \
                    and not is_in_past(point, hailstones[j]):
                intersections += 1

    return intersections

def find_stone(hailstones):
    s = Solver()

    # the unknowns: rock position and velocity
    px_rock, py_rock, pz_rock = Ints('px_rock py_rock pz_rock')
    vx_rock, vy_rock, vz_rock = Ints('vx_rock vy_rock vz_rock')

    for hs in hailstones[:4]:  # we only need 4 equations to solve the system
        px, py, pz = hs.position
        vx, vy, vz = hs.velocity

        # x/y/z equations for collision of rock and hailstone i
        s.add((px_rock - px) * (vy_rock - vy) == (py_rock - py) * (vx_rock - vx))
        s.add((px_rock - px) * (vz_rock - vz) == (pz_rock - pz) * (vx_rock - vx))
        s.add((py_rock - py) * (vy_rock - vy) == (py_rock - py) * (vy_rock - vy))

    if s.check() == sat:
        coords = s.model()[px_rock], s.model()[py_rock], s.model()[pz_rock]
        return [c.as_long() for c in coords]  # type: ignore

    assert False, "equation system was not solvable by z3"

class TestDay24(unittest.TestCase):
    def assert2d(self, actual, expected):
        if expected is None:
            self.assertIsNone(actual, f"expected parallel, got {actual}")
        else:
            self.assertAlmostEqual(actual[0], expected[0], places=2, msg="x")
            self.assertAlmostEqual(actual[1], expected[1], places=2, msg="y")

    def test_intersect2d_sample01_inside(self):
        a = Hailstone("19, 13, 30", "-2, 1, -2")
        b = Hailstone("18, 19, 22", "-1, -1, -2")

        intersection = intersect2d(a, b)

        self.assert2d(intersection, (14.333, 15.333))
        self.assertTrue(is_in_test_area(intersection))
        self.assertFalse(is_in_past(intersection, a))
        self.assertFalse(is_in_past(intersection, b))

    def test_intersect2d_sample02_inside(self):
        a = Hailstone("19, 13, 30", "-2, 1, -2")
        b = Hailstone("20, 25, 34", "-2, -2, -4")

        intersection = intersect2d(a, b)

        self.assert2d(intersection, (11.667, 16.667))
        self.assertTrue(is_in_test_area(intersection))
        self.assertFalse(is_in_past(intersection, a))
        self.assertFalse(is_in_past(intersection, b))

    def test_intersect2d_sample03_outside_test_area(self):
        a = Hailstone("19, 13, 30", "-2, 1, -2")
        b = Hailstone("12, 31, 28", "-1, -2, -1")

        intersection = intersect2d(a, b)

        self.assert2d(intersection, (6.2, 19.4))
        self.assertFalse(is_in_test_area(intersection))
        self.assertFalse(is_in_past(intersection, a))
        self.assertFalse(is_in_past(intersection, b))

    def test_intersect2d_sample04_past_of_a(self):
        a = Hailstone("19, 13, 30", "-2, 1, -2")
        b = Hailstone("20, 19, 15", "1, -5, -3")

        intersection = intersect2d(a, b)

        self.assert2d(intersection, (21.444, 11.777))
        self.assertTrue(is_in_test_area(intersection))
        self.assertTrue(is_in_past(intersection, a))
        self.assertFalse(is_in_past(intersection, b))

    def test_intersect2d_sample05_no_intersection(self):
        a = Hailstone("18, 19, 22", "-1, -1, -2")
        b = Hailstone("20, 25, 34", "-2, -2, -4")

        intersection = intersect2d(a, b)

        self.assert2d(intersection, None)

    def test_intersect2d_sample06_outside_test_area(self):
        a = Hailstone("18, 19, 22", "-1, -1, -2")
        b = Hailstone("12, 31, 28", "-1, -2, -1")

        intersection = intersect2d(a, b)

        self.assert2d(intersection, (-6, -5))
        self.assertFalse(is_in_test_area(intersection))
        self.assertFalse(is_in_past(intersection, a))
        self.assertFalse(is_in_past(intersection, b))


    def test_intersect2d_sample07_past_of_both(self):
        a = Hailstone("18, 19, 22", "-1, -1, -2")
        b = Hailstone("20, 19, 15", "1, -5, -3")

        intersection = intersect2d(a, b)

        self.assert2d(intersection, (19.666, 20.666))
        self.assertTrue(is_in_test_area(intersection))
        self.assertTrue(is_in_past(intersection, a))
        self.assertTrue(is_in_past(intersection, b))

    def test_intersect2d_sample08_outside_test_area(self):
        a = Hailstone("20, 25, 34", "-2, -2, -4")
        b = Hailstone("12, 31, 28", "-1, -2, -1")

        intersection = intersect2d(a, b)

        self.assert2d(intersection, (-2, 3))
        self.assertFalse(is_in_test_area(intersection))
        self.assertFalse(is_in_past(intersection, a))
        self.assertFalse(is_in_past(intersection, b))

    def test_intersect2d_sample09_past_of_b(self):
        a = Hailstone("20, 25, 34", "-2, -2, -4")
        b = Hailstone("20, 19, 15", "1, -5, -3")

        intersection = intersect2d(a, b)

        self.assert2d(intersection, (19, 24))
        self.assertTrue(is_in_test_area(intersection))
        self.assertFalse(is_in_past(intersection, a))
        self.assertTrue(is_in_past(intersection, b))

    def test_intersect2d_sample10_past_of_both(self):
        a = Hailstone("12, 31, 28", "-1, -2, -1")
        b = Hailstone("20, 19, 15", "1, -5, -3")

        intersection = intersect2d(a, b)

        self.assert2d(intersection, (16, 39))
        self.assertFalse(is_in_test_area(intersection))
        self.assertTrue(is_in_past(intersection, a))
        self.assertTrue(is_in_past(intersection, b))

def check(part, actual, expected=None):
    print(f"Part {part}{' (sample)' if is_sample else ''}: {actual} ", end="")
    if expected is None:
        print("❔")
    else:
        if actual != expected:
            print(f"≠ {expected} ❌")
            exit(1)
        print("✅")

if __name__ == '__main__':
    if is_sample:
        unittest.main(exit=False)
        print()

    hailstones = parse()

    part1 = count_intersections(hailstones)
    part2 = sum(find_stone(hailstones))

    check(1, part1,  2 if is_sample else 13754)
    check(2, part2, 47 if is_sample else 711031616315001)
