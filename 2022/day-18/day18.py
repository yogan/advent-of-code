import unittest, sys
import numpy as np
import matplotlib.pyplot as plt
import matplotlib
matplotlib.use('TkAgg')

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they scare the unittest module
is_sample = filename != "input.txt"


def parse():
    with open(filename) as f:
        lines = [line.strip() for line in f.readlines()]
        return [tuple(map(int, l.split(","))) for l in lines]

def update_ranges(ranges, coord, depth):
    if coord not in ranges:
        ranges[coord] = {(depth, depth)}
        return

    pillar = list(ranges[coord]) # working on a list copy, so that we can use indices

    i_after  = [(i, (l, r)) for i, (l, r) in enumerate(pillar) if l - 1 == depth]
    i_before = [(i, (l, r)) for i, (l, r) in enumerate(pillar) if r + 1 == depth]
    assert(len(i_before) <= 1 and len(i_after) <= 1)

    if len(i_before) == 1 and len(i_after) == 1:
        # Special case: depth is exactly between two existing ranges.
        # Join them into a single range.
        ai, after  = i_after[0]
        bi, before = i_before[0]
        # remove elements with indices ai and bi from pillar
        pillar = [pillar[i] for i in range(len(pillar)) if i != ai and i != bi]
        # add combined range to pillar
        pillar.append((before[0], after[1]))
        ranges[coord] = set(pillar)
        return

    if len(i_before) == 1:
        # append depth to the end of the range i_before
        bi, before = i_before[0]
        pillar[bi] = (before[0], depth)
        ranges[coord] = set(pillar)
        return

    if len(i_after) == 1:
        # prepend depth to the start of the range i_after
        ai, after = i_after[0]
        pillar[ai] = (depth, after[1])
        ranges[coord] = set(pillar)
        return

    # No adjacent ranges found, add a new range from depth to depth.
    ranges[coord].add((depth, depth))

def calc_ranges(cubes):
    xy_ranges = {}
    xz_ranges = {}
    zy_ranges = {}

    for cube in cubes:
        x, y, z = cube
        update_ranges(xy_ranges, (x, y), z)
        update_ranges(xz_ranges, (x, z), y)
        update_ranges(zy_ranges, (y, z), x)

    return (xy_ranges, xz_ranges, zy_ranges)

def count_sides(cubes):
    sides = 0
    xy_ranges, xz_ranges, zy_ranges = calc_ranges(cubes)

    for r in xy_ranges.values():
        sides += len(r) * 2
    for r in xz_ranges.values():
        sides += len(r) * 2
    for r in zy_ranges.values():
        sides += len(r) * 2

    return sides

def part1():
    cubes = parse()
    return count_sides(cubes)


class TestDay17(unittest.TestCase):
    def test_parse(self):
        cubes = parse()
        if is_sample:
            self.assertEqual(13, len(cubes))
            self.assertEqual((2, 2, 2), cubes[0])
            self.assertEqual((2, 3, 5), cubes[-1])
        else:
            self.assertEqual(2888, len(cubes))
            self.assertEqual((4, 14, 6), cubes[0])
            self.assertEqual((5, 14, 8), cubes[-1])

    def test_update_ranges(self):
        ranges = {}
        plane_coord = (1, 2)

        update_ranges(ranges, plane_coord, 3)
        self.assertEqual({(3, 3)}, ranges[plane_coord])

        update_ranges(ranges, plane_coord, 5)
        self.assertEqual({(3, 3), (5, 5)}, ranges[plane_coord])

        update_ranges(ranges, plane_coord, 4)
        self.assertEqual({(3, 5)}, ranges[plane_coord])

        update_ranges(ranges, plane_coord, 9)
        self.assertEqual({(3, 5), (9, 9)}, ranges[plane_coord])

        update_ranges(ranges, plane_coord, 10)
        self.assertEqual({(3, 5), (9, 10)}, ranges[plane_coord])

        update_ranges(ranges, plane_coord, 8)
        self.assertEqual({(3, 5), (8, 10)}, ranges[plane_coord])

        update_ranges(ranges, plane_coord, 1)
        self.assertEqual({(1, 1), (3, 5), (8, 10)}, ranges[plane_coord])

        update_ranges(ranges, plane_coord, 2)
        self.assertEqual({(1, 5), (8, 10)}, ranges[plane_coord])

    def test_count_sides_two_touching_cubes(self):
        cubes = [(2, 2, 2), (2, 2, 3)]
        sides = count_sides(cubes)
        self.assertEqual(4 * 2 + 2, sides)

    def test_count_sides_two_non_touching_cubes(self):
        cubes = [(2, 2, 2), (2, 2, 4)]
        sides = count_sides(cubes)
        self.assertEqual(2 * 6, sides)

    def test_count_sides_sample_step_by_step(self):
        # 2x1x1 cube
        # sides: 2 from top/bottom, 1 from front/back, 2 from left/right
        cubes = [(2, 2, 2), (1, 2, 2)]
        sides = count_sides(cubes)
        self.assertEqual(2+2 + 1+1 + 2+2, sides)

        # 3x1x1 cube
        # sides: 3 from top/bottom, 1 from front/back, 3 from left/right
        cubes.append((3, 2, 2))
        sides = count_sides(cubes)
        self.assertEqual(3+3 + 1+1 + 3+3, sides)

        # T tetris shape
        # sides: 4 from top/bottom, 2 from front/back, 3 from left/right
        cubes.append((2, 1, 2))
        sides = count_sides(cubes)
        self.assertEqual(4+4 + 2+2 + 3+3, sides)

        # + in a plane
        # sides: 5 from top/bottom, 3 from front/back, 3 from left/right
        cubes.append((2, 3, 2))
        sides = count_sides(cubes)
        self.assertEqual(5+5 + 3+3 + 3+3, sides)

        # + with one cube below the center
        # sides: 5 from top/bottom, 4 from front/back, 4 from left/right
        cubes.append((2, 2, 1))
        sides = count_sides(cubes)
        self.assertEqual(5+5 + 4+4 + 4+4, sides)

        # + with one cube each below and above the center
        # sides: 5 from top/bottom, 5 from front/back, 5 from left/right
        cubes.append((2, 2, 3))
        sides = count_sides(cubes)
        self.assertEqual(5+5 + 5+5 + 5+5, sides)

        # one more onto the top cube
        # sides: 5 from top/bottom, 6 from front/back, 6 from left/right
        cubes.append((2, 2, 4))
        sides = count_sides(cubes)
        self.assertEqual(5+5 + 6+6 + 6+6, sides)

        # now add one cube "floating" above the top (gap of 1)
        # sides:
        #   - 6 from top/bottom (5 from the plus, 1 from single floating cube)
        #   - 7 from front/back, 7 from left/right
        cubes.append((2, 2, 6))
        sides = count_sides(cubes)
        self.assertEqual(6+6 + 7+7 + 7+7, sides)

    def test_calc_ranges_plus_shape(self):
        # + shape lying in the z = 2 plane
        cubes = [(2, 2, 2), (1, 2, 2), (3, 2, 2), (2, 1, 2), (2, 3, 2)]
        xy_ranges, xz_ranges, zy_ranges = calc_ranges(cubes)

        # + from above
        self.assertEqual(5, len(xy_ranges))
        self.assertEqual({(2, 2)}, xy_ranges[(2, 1)])
        self.assertEqual({(2, 2)}, xy_ranges[(1, 2)])
        self.assertEqual({(2, 2)}, xy_ranges[(2, 2)])
        self.assertEqual({(2, 2)}, xy_ranges[(3, 2)])
        self.assertEqual({(2, 2)}, xy_ranges[(2, 3)])

        # from front
        self.assertEqual(3, len(xz_ranges))
        self.assertEqual({(2, 2)}, xz_ranges[(1, 2)])
        self.assertEqual({(1, 3)}, xz_ranges[(2, 2)])
        self.assertEqual({(2, 2)}, xz_ranges[(3, 2)])

        # from side
        self.assertEqual(3, len(zy_ranges))
        self.assertEqual({(2, 2)}, zy_ranges[(1, 2)])
        self.assertEqual({(1, 3)}, zy_ranges[(2, 2)])
        self.assertEqual({(2, 2)}, zy_ranges[(3, 2)])

    def test_calc_ranges_plus_shape(self):
        # + shape with one cube below and two cubes on top of the center
        cubes = [(2, 2, 2), (1, 2, 2), (3, 2, 2), (2, 1, 2), (2, 3, 2),
                 (2, 2, 1), (2, 2, 3), (2, 2, 4)]
        xy_ranges, xz_ranges, zy_ranges = calc_ranges(cubes)

        # + from above
        self.assertEqual(5, len(xy_ranges))
        self.assertEqual({(2, 2)}, xy_ranges[(2, 1)])
        self.assertEqual({(2, 2)}, xy_ranges[(1, 2)])
        self.assertEqual({(1, 4)}, xy_ranges[(2, 2)])
        self.assertEqual({(2, 2)}, xy_ranges[(3, 2)])
        self.assertEqual({(2, 2)}, xy_ranges[(2, 3)])

        # from front
        self.assertEqual(6, len(xz_ranges))
        self.assertEqual({(2, 2)}, xz_ranges[(1, 2)])
        self.assertEqual({(2, 2)}, xz_ranges[(2, 1)])
        self.assertEqual({(1, 3)}, xz_ranges[(2, 2)])
        self.assertEqual({(2, 2)}, xz_ranges[(2, 3)])
        self.assertEqual({(2, 2)}, xz_ranges[(2, 4)])
        self.assertEqual({(2, 2)}, xz_ranges[(3, 2)])

        # from side
        self.assertEqual(6, len(zy_ranges))
        self.assertEqual({(2, 2)}, zy_ranges[(1, 2)])
        self.assertEqual({(2, 2)}, zy_ranges[(2, 1)])
        self.assertEqual({(1, 3)}, zy_ranges[(2, 2)])
        self.assertEqual({(2, 2)}, zy_ranges[(2, 3)])
        self.assertEqual({(2, 2)}, zy_ranges[(2, 4)])
        self.assertEqual({(2, 2)}, zy_ranges[(3, 2)])

    def test_part1(self):
        self.assertEqual(64 if is_sample else 4244, part1())


if __name__ == '__main__':
    unittest.main(exit=False)
    print()

    res1 = part1()
    print(f"Part 1: {res1}", "(sample)" if is_sample else "")


# Plotting stuff if required

def draw_voxels(cubes, size=8):
    n_voxels = np.zeros((size, size, size), dtype=bool)

    for cx, cy, cz in cubes:
        n_voxels[cx, cy, cz] = True

    ax = plt.figure().add_subplot(projection='3d')
    ax.voxels(n_voxels, edgecolor="k")
    plt.show()

# if is_sample:
#     draw_voxels(parse()[:9])

# if not is_sample:
#     draw_voxels(parse(), 30)
