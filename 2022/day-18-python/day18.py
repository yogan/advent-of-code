import sys
import unittest

from tqdm import tqdm


def draw_voxels(cube_groups, size=8):
    assert len(cube_groups) <= 2, "Can only draw up to two groups"

    import matplotlib
    import matplotlib.pyplot as plt
    import numpy as np

    matplotlib.use("TkAgg")

    voxel_groups = [
        np.zeros((size, size, size), dtype=bool) for _ in range(len(cube_groups))
    ]

    for i, cubes in enumerate(cube_groups):
        for cx, cy, cz in cubes:
            voxel_groups[i][cx, cy, cz] = True

    ax = plt.figure().add_subplot(projection="3d")

    facecolors = ["#FFD65D66", "#7A88CCDD"]
    edgecolors = ["#BFAB6E", "#7D84A6"]
    for i, voxels in enumerate(voxel_groups):
        ax.voxels(voxels, facecolors=facecolors[i], edgecolor=edgecolors[i])  # type: ignore

    plt.show()


if len(sys.argv) < 2:
    print("Missing input file.")
    sys.exit(1)
filename = sys.argv[1]
is_sample = filename != "input.txt"
visualize = "-v" in sys.argv or "--visualize" in sys.argv


def parse(filename=filename):
    with open(filename) as f:
        lines = [line.strip() for line in f.readlines()]
        return [tuple(map(int, l.split(","))) for l in lines]


def update_ranges(ranges, coord, depth):
    if coord not in ranges:
        ranges[coord] = {(depth, depth)}
        return

    pillar = list(ranges[coord])  # working on a list copy, so that we can use indices

    i_after = [(i, (l, r)) for i, (l, r) in enumerate(pillar) if l - 1 == depth]
    i_before = [(i, (l, r)) for i, (l, r) in enumerate(pillar) if r + 1 == depth]
    assert len(i_before) <= 1 and len(i_after) <= 1, (i_before, i_after)

    if len(i_before) == 1 and len(i_after) == 1:
        # Special case: depth is exactly between two existing ranges.
        # Join them into a single range.
        ai, after = i_after[0]
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
    yz_ranges = {}

    for cube in cubes:
        x, y, z = cube
        update_ranges(xy_ranges, (x, y), z)
        update_ranges(xz_ranges, (x, z), y)
        update_ranges(yz_ranges, (y, z), x)

    return (xy_ranges, xz_ranges, yz_ranges)


def count_sides(ranges):
    sides = 0
    xy_ranges, xz_ranges, yz_ranges = ranges

    for r in xy_ranges.values():
        sides += len(r) * 2
    for r in xz_ranges.values():
        sides += len(r) * 2
    for r in yz_ranges.values():
        sides += len(r) * 2

    return sides


def sort_pillar(pillar):
    plist = list(pillar)
    plist.sort(key=lambda x: x[0])
    return plist


def dimensions(cubes):
    x_min = min(cubes, key=lambda c: c[0])[0]
    x_max = max(cubes, key=lambda c: c[0])[0]
    y_min = min(cubes, key=lambda c: c[1])[1]
    y_max = max(cubes, key=lambda c: c[1])[1]
    z_min = min(cubes, key=lambda c: c[2])[2]
    z_max = max(cubes, key=lambda c: c[2])[2]
    return (x_min, x_max, y_min, y_max, z_min, z_max)


def find_air_candidates(ranges):
    xy_ranges, xz_ranges, yz_ranges = ranges

    # candidates to check: all cubes between the range intervals
    candidates = set()

    for (x, y), pillar in xy_ranges.items():
        if len(pillar) == 1:
            continue
        pillar = sort_pillar(pillar)
        for i in range(len(pillar) - 1):
            r1_end = pillar[i][1]
            r2_start = pillar[i + 1][0]
            candidates |= {(x, y, z) for z in range(r1_end + 1, r2_start)}

    for (x, z), pillar in xz_ranges.items():
        if len(pillar) == 1:
            continue
        pillar = sort_pillar(pillar)
        for i in range(len(pillar) - 1):
            r1_end = pillar[i][1]
            r2_start = pillar[i + 1][0]
            candidates |= {(x, y, z) for y in range(r1_end + 1, r2_start)}

    for (y, z), pillar in yz_ranges.items():
        if len(pillar) == 1:
            continue
        pillar = sort_pillar(pillar)
        for i in range(len(pillar) - 1):
            r1_end = pillar[i][1]
            r2_start = pillar[i + 1][0]
            candidates |= {(x, y, z) for x in range(r1_end + 1, r2_start)}

    return candidates


def grows_to_edge(cube, cubes, air_cubes):
    def get_neighbors(cube):
        x, y, z = cube
        return {
            (x - 1, y, z),
            (x + 1, y, z),
            (x, y - 1, z),
            (x, y + 1, z),
            (x, y, z - 1),
            (x, y, z + 1),
        }

    def on_edge(cube, dim):
        x, y, z = cube
        x_min, x_max, y_min, y_max, z_min, z_max = dim
        return (
            x == x_min
            or x == x_max
            or y == y_min
            or y == y_max
            or z == z_min
            or z == z_max
        )

    if cube in air_cubes:
        # already found in some other run, no need to check again
        return False

    x, y, z = cube
    dim = dimensions(cubes)
    x_min, x_max, y_min, y_max, z_min, z_max = dim
    assert (
        x_min <= x <= x_max and y_min <= y <= y_max and z_min <= z <= z_max
    ), f"initial cube {cube} already outside of dimensions {dim}"

    border = {cube}
    visited = set(cubes).union(air_cubes, border)

    while len(border) > 0:
        if any(on_edge(c, dim) for c in border):
            return True

        new_border = set()

        for border_cube in border:
            for neighbor in get_neighbors(border_cube):
                if neighbor not in visited:
                    new_border.add(neighbor)

        border = new_border
        visited |= border

    return False


def find_air_cubes(cubes, ranges):
    if len(cubes) == 0:
        return set()

    candidates = find_air_candidates(ranges)

    # NOTE: 1593 candidates for real data; enough for a progress bar, but
    # actually pretty fast (due to the visited checks in grows_to_edge)
    c_iter = candidates if len(candidates) < 1000 else tqdm(candidates)

    air_cubes = set()
    for c in c_iter:
        if not grows_to_edge(c, cubes, air_cubes):
            air_cubes.add(c)
    return air_cubes


def part1():
    cubes = parse()
    ranges = calc_ranges(cubes)

    return count_sides(ranges)


def part2():
    cubes = parse()

    ranges = calc_ranges(cubes)
    sides = count_sides(ranges)

    air_cubes = find_air_cubes(cubes, ranges)
    air_ranges = calc_ranges(air_cubes)
    air_sides = count_sides(air_ranges)

    if visualize and not is_sample:
        draw_voxels([list(air_cubes)], 30)  # input_air_pockets.png

    return sides - air_sides


class TestDay18(unittest.TestCase):
    global draw_voxels

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
        ranges = calc_ranges(cubes)
        sides = count_sides(ranges)
        self.assertEqual(4 * 2 + 2, sides)

    def test_count_sides_two_non_touching_cubes(self):
        cubes = [(2, 2, 2), (2, 2, 4)]
        ranges = calc_ranges(cubes)
        sides = count_sides(ranges)
        self.assertEqual(2 * 6, sides)

    def test_count_sides_sample_step_by_step(self):
        # 2x1x1 cube
        # sides: 2 from top/bottom, 1 from front/back, 2 from left/right
        cubes = [(2, 2, 2), (1, 2, 2)]
        ranges = calc_ranges(cubes)
        sides = count_sides(ranges)
        self.assertEqual(2 + 2 + 1 + 1 + 2 + 2, sides)

        # 3x1x1 cube
        # sides: 3 from top/bottom, 1 from front/back, 3 from left/right
        cubes.append((3, 2, 2))
        ranges = calc_ranges(cubes)
        sides = count_sides(ranges)
        self.assertEqual(3 + 3 + 1 + 1 + 3 + 3, sides)

        # T tetris shape
        # sides: 4 from top/bottom, 2 from front/back, 3 from left/right
        cubes.append((2, 1, 2))
        ranges = calc_ranges(cubes)
        sides = count_sides(ranges)
        self.assertEqual(4 + 4 + 2 + 2 + 3 + 3, sides)

        # + in a plane
        # sides: 5 from top/bottom, 3 from front/back, 3 from left/right
        cubes.append((2, 3, 2))
        ranges = calc_ranges(cubes)
        sides = count_sides(ranges)
        self.assertEqual(5 + 5 + 3 + 3 + 3 + 3, sides)

        # + with one cube below the center
        # sides: 5 from top/bottom, 4 from front/back, 4 from left/right
        cubes.append((2, 2, 1))
        ranges = calc_ranges(cubes)
        sides = count_sides(ranges)
        self.assertEqual(5 + 5 + 4 + 4 + 4 + 4, sides)

        # + with one cube each below and above the center
        # sides: 5 from top/bottom, 5 from front/back, 5 from left/right
        cubes.append((2, 2, 3))
        ranges = calc_ranges(cubes)
        sides = count_sides(ranges)
        self.assertEqual(5 + 5 + 5 + 5 + 5 + 5, sides)

        # one more onto the top cube
        # sides: 5 from top/bottom, 6 from front/back, 6 from left/right
        cubes.append((2, 2, 4))
        ranges = calc_ranges(cubes)
        sides = count_sides(ranges)
        self.assertEqual(5 + 5 + 6 + 6 + 6 + 6, sides)

        # now add one cube "floating" above the top (gap of 1)
        # sides:
        #   - 6 from top/bottom (5 from the plus, 1 from single floating cube)
        #   - 7 from front/back, 7 from left/right
        cubes.append((2, 2, 6))
        ranges = calc_ranges(cubes)
        sides = count_sides(ranges)
        self.assertEqual(6 + 6 + 7 + 7 + 7 + 7, sides)

    def test_calc_ranges_plus_shape(self):
        # + shape lying in the z = 2 plane
        cubes = [(2, 2, 2), (1, 2, 2), (3, 2, 2), (2, 1, 2), (2, 3, 2)]
        xy_ranges, xz_ranges, yz_ranges = calc_ranges(cubes)

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
        self.assertEqual(3, len(yz_ranges))
        self.assertEqual({(2, 2)}, yz_ranges[(1, 2)])
        self.assertEqual({(1, 3)}, yz_ranges[(2, 2)])
        self.assertEqual({(2, 2)}, yz_ranges[(3, 2)])

    def test_calc_ranges_plus_shapes(self):
        # + shape with one cube below and two cubes on top of the center
        cubes = [
            (2, 2, 2),
            (1, 2, 2),
            (3, 2, 2),
            (2, 1, 2),
            (2, 3, 2),
            (2, 2, 1),
            (2, 2, 3),
            (2, 2, 4),
        ]
        xy_ranges, xz_ranges, yz_ranges = calc_ranges(cubes)

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
        self.assertEqual(6, len(yz_ranges))
        self.assertEqual({(2, 2)}, yz_ranges[(1, 2)])
        self.assertEqual({(2, 2)}, yz_ranges[(2, 1)])
        self.assertEqual({(1, 3)}, yz_ranges[(2, 2)])
        self.assertEqual({(2, 2)}, yz_ranges[(2, 3)])
        self.assertEqual({(2, 2)}, yz_ranges[(2, 4)])
        self.assertEqual({(2, 2)}, yz_ranges[(3, 2)])

    def test_sort_pillar(self):
        pillar = {(1, 2), (9, 13), (5, 5), (7, 8)}
        self.assertEqual([(1, 2), (5, 5), (7, 8), (9, 13)], sort_pillar(pillar))

    def test_dimensions(self):
        cubes = parse("sample.txt")
        self.assertEqual((1, 3, 1, 3, 1, 6), dimensions(cubes))

    def test_find_air_candidates(self):
        self.assertEqual(set(), find_air_candidates(calc_ranges([])))

        cubes = [(2, 2, 1), (2, 2, 3), (2, 2, 6)]
        self.assertEqual(
            {(2, 2, 2), (2, 2, 4), (2, 2, 5)}, find_air_candidates(calc_ranges(cubes))
        )

        sample_cubes = parse("sample.txt")
        droplet_with_open_air_hole = sample_cubes[:-1]
        droplet_with_closed_air_hole = sample_cubes

        self.assertEqual(
            {
                (1, 2, 3),
                (2, 1, 3),
                (3, 2, 3),
                (1, 2, 4),
                (2, 1, 4),
                (3, 2, 4),
                (2, 2, 5),
            },
            find_air_candidates(calc_ranges(droplet_with_open_air_hole)),
        )

        self.assertEqual(
            {
                (1, 2, 3),
                (2, 1, 3),
                (2, 3, 3),
                (3, 2, 3),
                (1, 2, 4),
                (2, 1, 4),
                (2, 3, 4),
                (3, 2, 4),
                (2, 2, 5),
            },
            find_air_candidates(calc_ranges(droplet_with_closed_air_hole)),
        )

    def test_find_air_cubes_empty(self):
        cubes = []
        ranges = calc_ranges(cubes)
        self.assertEqual(set(), find_air_cubes(cubes, ranges))

    def test_find_air_cubes_three_separate_cubes(self):
        cubes = [(2, 2, 1), (2, 2, 3), (2, 2, 6)]
        ranges = calc_ranges(cubes)
        self.assertEqual(set(), find_air_cubes(cubes, ranges))

    def test_find_air_cubes_long_tube_one_end_open(self):
        cubes = [
            (2, 2, 0),
            (1, 2, 1),
            (3, 2, 1),
            (2, 1, 1),
            (2, 3, 1),
            (1, 2, 2),
            (3, 2, 2),
            (2, 1, 2),
            (2, 3, 2),
            (1, 2, 3),
            (3, 2, 3),
            (2, 1, 3),
            (2, 3, 3),
            (1, 2, 4),
            (3, 2, 4),
            (2, 1, 4),
            (2, 3, 4),
        ]
        ranges = calc_ranges(cubes)
        self.assertEqual(set(), find_air_cubes(cubes, ranges))

    def test_find_air_cubes_long_tube_closed(self):
        cubes = [
            (4, 4, 0),
            (3, 4, 1),
            (5, 4, 1),
            (4, 3, 1),
            (4, 5, 1),
            (3, 4, 2),
            (5, 4, 2),
            (4, 3, 2),
            (4, 5, 2),
            (3, 4, 3),
            (5, 4, 3),
            (4, 3, 3),
            (4, 5, 3),
            (3, 4, 4),
            (5, 4, 4),
            (4, 3, 4),
            (4, 5, 4),
            (4, 4, 5),
        ]
        expected_air_cubes = {(4, 4, 1), (4, 4, 2), (4, 4, 3), (4, 4, 4)}
        # air_pocket_long_tube.png
        # if is_sample:
        #     draw_voxels([cubes, expected_air_cubes])
        ranges = calc_ranges(cubes)
        self.assertEqual(expected_air_cubes, find_air_cubes(cubes, ranges))

    def test_find_air_cubes_sample_droplet_open(self):
        cubes = parse("sample.txt")[:-1]
        ranges = calc_ranges(cubes)
        self.assertEqual(set(), find_air_cubes(cubes, ranges))

    def test_find_air_cubes_sample_droplet_closed(self):
        cubes = parse("sample.txt")
        ranges = calc_ranges(cubes)
        self.assertEqual({(2, 2, 5)}, find_air_cubes(cubes, ranges))

    def test_part1(self):
        self.assertEqual(64 if is_sample else 4244, part1())

    def test_part2(self):
        if is_sample:
            self.assertEqual(58 if is_sample else 2460, part2())


if __name__ == "__main__":
    unittest.main(argv=sys.argv[:1], exit=False)
    print()

    res1 = part1()
    print(f"Part 1: {res1}", "(sample)" if is_sample else "")

    res2 = part2()
    print(f"Part 2: {res2}", "(sample)" if is_sample else "")


if visualize:
    if is_sample:
        draw_voxels([parse()[:9], [(2, 2, 5)]])  # sample_partial.png
        draw_voxels([parse(), [(2, 2, 5)]])  # sample_air_pocket.png
    else:
        draw_voxels([parse()], 30)  # input.png
