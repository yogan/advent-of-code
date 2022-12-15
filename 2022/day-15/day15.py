import unittest, sys

file = sys.argv[1] if len(sys.argv) > 1 else "day15.in"
sys.argv = sys.argv[:1] # strip args, they scare the unittest module
is_sample = file != "day15.in"

def parse():
    with open(file) as f:
        lines = [line.replace("Sensor at ", "")
                    .replace(" closest beacon is at ", "")
                for line in f.read().splitlines()]

    coords = [tuple(map(lambda x: x.split(", "), line.split(":")))
              for line in lines]

    pairs = set()
    for sensor, beacon in coords:
        sensor = tuple(map(lambda n: int(n.split("=")[1]), sensor))
        beacon = tuple(map(lambda n: int(n.split("=")[1]), beacon))
        pairs.add((sensor, beacon))

    return pairs

def calc_manhattan_dist(sensor, beacon):
    sx, sy = sensor
    bx, by = beacon
    return abs(sx - bx) + abs(sy - by)

def find_sensor_radii(sensor_beacon_pairs):
    radii = set()
    for sensor, beacon in sensor_beacon_pairs:
        radii.add((sensor[0], sensor[1], calc_manhattan_dist(sensor, beacon)))
    return radii

def calc_line_coverage(sensor, sensor_radius, line_y):
    assert(sensor_radius > 0)
    line_radius = sensor_radius - abs(sensor[1] - line_y)
    if (line_radius < 0):
        return None
    return (sensor[0] - line_radius, sensor[0] + line_radius)

def get_line_coverages(radii, line_y):
    ranges = set()
    for x, y, radius in radii:
        coverage = calc_line_coverage((x, y), radius, line_y)
        if (coverage is not None):
            ranges.add(coverage)
    return ranges

def simplify_line_coverages(coverages):
    sorted_coverages = sorted(coverages, key=lambda c: c[0])
    simplified = set()
    for coverage in sorted_coverages:
        if (len(simplified) == 0):
            simplified.add(coverage)
        else:
            last = simplified.pop()
            if (last[1] >= coverage[0]):
                simplified.add((last[0], max(last[1], coverage[1])))
            else:
                simplified.add(last)
                simplified.add(coverage)
    return simplified

def count_positions(coverages):
    count = 0
    for start, end in coverages:
        count += (end - start + 1)
    return count

def find_distress_beacon(coverages, x_min, x_max):
    for x in range(x_min, x_max + 1):
        for left, right in coverages:
            found = False
            if (left <= x and x <= right):
                found = True
                break
        if (not found):
            return x
    return None

def part1():
    sensor_beacon_pairs = parse()
    radii = find_sensor_radii(sensor_beacon_pairs)
    y = 10 if is_sample else 2000000
    coverages = get_line_coverages(radii, y)
    coverages = simplify_line_coverages(coverages)
    return count_positions(coverages) - 1

def part2():
    sensor_beacon_pairs = parse()
    radii = find_sensor_radii(sensor_beacon_pairs)
    xy_min, xy_max = 0, 20 if is_sample else 4000000
    if (not is_sample):
        print(f"Iterating all {xy_max} is too slow, aborting.")
        return None
    for y in range(xy_min, xy_max + 1):
        if (not is_sample and y % 100 == 0):
            print("checking y = {}…".format(y))
        coverages = get_line_coverages(radii, y)
        coverages = simplify_line_coverages(coverages)
        distress_beacon_x = find_distress_beacon(coverages, xy_min, xy_max)
        if (distress_beacon_x is not None):
            return distress_beacon_x * 4000000 + y
    raise Exception("No distress beacon found")

class TestDay15(unittest.TestCase):
    def test_calc_manhattan_dist(self):
        self.assertEqual(0, calc_manhattan_dist((0, 0), (0, 0)))

        self.assertEqual(1, calc_manhattan_dist((0, 0), ( 0,  1)))
        self.assertEqual(1, calc_manhattan_dist((0, 0), ( 1,  0)))
        self.assertEqual(1, calc_manhattan_dist((0, 0), ( 0, -1)))
        self.assertEqual(1, calc_manhattan_dist((0, 0), (-1,  0)))

        self.assertEqual(2, calc_manhattan_dist((0, 0), ( 1,  1)))
        self.assertEqual(2, calc_manhattan_dist((0, 0), ( 1, -1)))
        self.assertEqual(2, calc_manhattan_dist((0, 0), (-1,  1)))
        self.assertEqual(2, calc_manhattan_dist((0, 0), (-1, -1)))

        self.assertEqual(9, calc_manhattan_dist((8, 7), (2, 10)))

    def test_find_sensor_radii(self):
        pairs = {((2, 18), (-2, 15)), ((9, 16), (10, 16)), ((8, 7), (2, 10))}
        expected_radii = {(2, 18, 7), (9, 16, 1), (8, 7, 9)}
        self.assertEqual(expected_radii, find_sensor_radii(pairs))

    def test_calc_line_coverage(self):
        self.assertEqual(None,   calc_line_coverage((4,  8), 1, 10))
        self.assertEqual((4, 4), calc_line_coverage((4,  9), 1, 10))
        self.assertEqual((3, 5), calc_line_coverage((4, 10), 1, 10))

    def test_get_line_coverages(self):
        radii = {(5, 10, 2), (9, 16, 1), (8, 7, 9)}
        expected_coverages = {(3, 7), (2, 14)}
        coverages = get_line_coverages(radii, 10)
        self.assertEqual(expected_coverages, coverages)

    def test_simplify_line_coverages(self):
        coverages = {(3, 7), (2, 14)}
        expected_coverages = {(2, 14)}
        coverages = simplify_line_coverages(coverages)
        self.assertEqual(expected_coverages, coverages)

        coverages = {(3, 7), (2, 14), (4, 15), (17, 19), (1, 9)}
        expected_coverages = {(1, 15), (17, 19)}
        coverages = simplify_line_coverages(coverages)
        self.assertEqual(expected_coverages, coverages)

    def test_count_positions(self):
        self.assertEqual(15 + 3, count_positions({(1, 15), (17, 19)}))

    def test_find_distress_beacon(self):
        x_min, x_max = 0, 20

        self.assertEqual(None, find_distress_beacon({(-8, 26)}, x_min, x_max))
        self.assertEqual(None, find_distress_beacon({(0, 20)}, x_min, x_max))
        self.assertEqual(0, find_distress_beacon({(1, 20)}, x_min, x_max))
        self.assertEqual(20, find_distress_beacon({(0, 19)}, x_min, x_max))

        self.assertEqual(None, find_distress_beacon({(-1, 25), (-3, 13)}, x_min, x_max))
        self.assertEqual(None, find_distress_beacon({(-3, 13), (-1, 25)}, x_min, x_max))
        self.assertEqual(14, find_distress_beacon({(15, 25), (-3, 13)}, x_min, x_max))
        self.assertEqual(14, find_distress_beacon({(-3, 13), (15, 25)}, x_min, x_max))

    def test_part1(self):
        self.assertEqual(26 if is_sample else 5832528, part1())

    def test_part2(self):
        self.assertEqual(56000011 if is_sample else None, part2())

if __name__ == '__main__':
    unittest.main(exit=False)

    res1 = part1()
    print(f"Part 1: {res1}", "(sample)" if is_sample else "")
    res2 = part2()
    print(f"Part 2: {res2}", "(sample)" if is_sample else "")
