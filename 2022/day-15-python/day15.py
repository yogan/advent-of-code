import sys
import unittest
from tqdm import tqdm

file = sys.argv[1] if len(sys.argv) > 1 else "day15.in"
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

def check_surroundings(radii, xy_min, xy_max, is_unit_test = False):
    if (not is_unit_test):
        i = 1
    else:
        s = set()

    for cx, cy, radius in radii:
        if (not is_unit_test and not is_sample):
            print(f"Checking beacon area {i} of {len(radii)}")
            i += 1

        s_radius = radius + 1
        x_min = min(cx - s_radius, xy_min) + 1
        x_range = range(cx - s_radius, cx + s_radius + 1) \
            if is_sample or is_unit_test \
            else tqdm(range(cx - s_radius, cx + s_radius + 1))

        for x in x_range:
            dist = s_radius - abs(cx - x)
            if (xy_min <= x and x <= xy_max):
                ys = [cy - dist - 1, cy - dist, cy + dist, cy + dist + 1]
                for y in ys:
                    if (xy_min <= y and y <= xy_max):
                        if (is_unit_test):
                            s.add((x, y))
                        elif (not is_covered((x, y), radii)):
                            return (x, y)

        if (is_unit_test):
            # remove "peaks" at top/bottom
            top_peak = (cx, cy - s_radius - 1)
            btm_peak = (cx, cy + s_radius + 1)
            if top_peak in s:
                s.remove(top_peak)
            if btm_peak in s:
                s.remove(btm_peak)

    if (is_unit_test):
        return s
    else:
        raise Exception("no uncovered point found")

def is_covered(point, radii):
    for cx, cy, radius in radii:
        dist = calc_manhattan_dist(point, (cx, cy))
        if (dist <= radius):
            return True
    return False

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
    limit = 4000000
    xy_min, xy_max = 0, 20 if is_sample else limit
    x, y = check_surroundings(radii, xy_min, xy_max)
    print(f"Distress signal at x={x}, y={y}")
    return x * limit + y

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

    def test_check_surroundings(self):
        is_unit_test = True

        #    012345
        # 0...sss...
        # 1..ss#ss..
        # 2..s#S#s..
        # 3..ss#ss..
        # 4...sss...
        radii = {(2, 2, 1)}
        self.assertEqual({
                        (1, 0), (2, 0), (3, 0),
                (0, 1), (1, 1),         (3, 1), (4, 1),
                (0, 2),                         (4, 2),
                (0, 3), (1, 3),         (3, 3), (4, 3),
                        (1, 4), (2, 4), (3, 4),
            }, check_surroundings(radii, 0, 10, is_unit_test))

        #     012345678
        # 1 .....sss......
        # 2 ....ss#ss.....
        # 3 ...ss###ss....
        # 4 ...s##S##s....
        # 5 ...ss###ss....
        # 6 ....ss#ss.....
        # 7 .....sss......
        radii = {(4, 4, 2)}
        self.assertEqual({  (3, 1), (4, 1), (5, 1),
                    (2, 2), (3, 2),         (5, 2), (6, 2),
            (1, 3), (2, 3),                         (6, 3), (7, 3),
            (1, 4),                                         (7, 4),
            (1, 5), (2, 5),                         (6, 5), (7, 5),
                    (2, 6), (3, 6),         (5, 6), (6, 6),
                            (3, 7), (4, 7), (5, 7),
            }, check_surroundings(radii, 0, 10, is_unit_test))

        #     012345678
        # 1 .............. cutoff top and left at xy = 3
        # 2 ..............
        # 3 ....+---------
        # 4 ....|#S##s....
        # 5 ....|###ss....
        # 6 ....|s#ss.....
        # 7 ....|sss......
        radii = {(4, 4, 2)}
        self.assertEqual({
                                                    (6, 3), (7, 3),
                                                            (7, 4),
                                                    (6, 5), (7, 5),
                            (3, 6),         (5, 6), (6, 6),
                            (3, 7), (4, 7), (5, 7),
            }, check_surroundings(radii, 3, 10, is_unit_test))

        #     0123456
        # 1 .....s|... cutoff right and bottom at xy = 3
        # 2 ....ss|...
        # 3 ...ss#|...
        # 4 ------+...
        radii = {(4, 4, 2)}
        self.assertEqual({  (3, 1),
                    (2, 2), (3, 2),
            (1, 3), (2, 3),
            }, check_surroundings(radii, 0, 3, is_unit_test))

        #     012345678
        # 0 ...sss....... two overlapping areas merged
        # 1 ..ss#sssss...
        # 2 ..s#S#ss#ss..
        # 3 ..ss#ss#S#s..
        # 4 ...sssss#ss..
        # 5 .......sss...
        radii = {(2, 2, 1), (6, 3, 1)}
        self.assertEqual({
                    (1, 0), (2, 0), (3, 0),
            (0, 1), (1, 1),         (3, 1), (4, 1), (5, 1), (6, 1), (7, 1),
            (0, 2),                         (4, 2), (5, 2),         (7, 2), (8, 2),
            (0, 3), (1, 3),         (3, 3), (4, 3),                         (8, 3),
                    (1, 4), (2, 4), (3, 4), (4, 4), (5, 4),         (7, 4), (8, 4),
                                                    (5, 5), (6, 5), (7, 5),
            }, check_surroundings(radii, 0, 10, is_unit_test))

        #     012345678
        # 0 ............. two overlapping areas merged with cutoff
        # 1 .............
        # 2 ....+--------
        # 3 ....|ss#S#s..
        # 4 ....|sss#ss..
        # 5 ....|..sss...
        radii = {(2, 2, 1), (6, 3, 1)}
        self.assertEqual({
                (3, 3), (4, 3),                         (8, 3),
                (3, 4), (4, 4), (5, 4),         (7, 4), (8, 4),
                                (5, 5), (6, 5), (7, 5),
            }, check_surroundings(radii, 3, 10, is_unit_test))

    def test_is_covered(self):
        #     012345678
        # 0 .............
        # 1 ....#........
        # 2 ...#S#..#....
        # 3 ....#..#S#...
        # 4 ........#....
        # 5 .............
        radii = {(2, 2, 1), (6, 3, 1)}

        self.assertTrue(is_covered((2, 1), radii))
        self.assertTrue(is_covered((1, 2), radii))
        self.assertTrue(is_covered((2, 2), radii))
        self.assertTrue(is_covered((3, 2), radii))
        self.assertTrue(is_covered((2, 3), radii))
        self.assertFalse(is_covered((1, 1), radii))
        self.assertFalse(is_covered((3, 1), radii))
        self.assertFalse(is_covered((0, 2), radii))
        self.assertFalse(is_covered((4, 2), radii))
        self.assertFalse(is_covered((1, 3), radii))
        self.assertFalse(is_covered((3, 3), radii))

        self.assertTrue(is_covered((6, 2), radii))
        self.assertTrue(is_covered((5, 3), radii))
        self.assertTrue(is_covered((6, 3), radii))
        self.assertTrue(is_covered((7, 3), radii))
        self.assertTrue(is_covered((6, 4), radii))
        self.assertFalse(is_covered((5, 2), radii))
        self.assertFalse(is_covered((7, 2), radii))
        self.assertFalse(is_covered((4, 3), radii))
        self.assertFalse(is_covered((8, 3), radii))
        self.assertFalse(is_covered((5, 4), radii))
        self.assertFalse(is_covered((7, 4), radii))

    def test_part1(self):
        self.assertEqual(26 if is_sample else 5832528, part1())

    # no unit test for part 2, but asserts in main below

if __name__ == '__main__':
    unittest.main(argv=sys.argv[:1], exit=False)
    print()

    res1 = part1()
    print(f"Part 1: {res1}", "(sample)" if is_sample else "")
    print()
    res2 = part2()
    assert(res2 == 56000011 if is_sample else 13360899249595)
    print(f"Part 2: {res2}", "(sample)" if is_sample else "")
