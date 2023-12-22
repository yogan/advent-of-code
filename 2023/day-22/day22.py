import sys, unittest
from collections import deque

# no tqdm in Docker image (CI), but we actually don't want it there anyway
try:
    from tqdm import tqdm # type: ignore
except ImportError:
    def tqdm(iterable):
        return iterable

if len(sys.argv) == 3 and "--visualize" in sys.argv:
    sys.argv.remove("--visualize")
    visualize = True
else:
    visualize = False
if len(sys.argv) != 2:
    print("Missing input file.")
    exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they confuse the unittest module
is_sample = filename.startswith("sample")

def to_coords(left, right):
    l = tuple(map(int, left.split(",")))
    r = tuple(map(int, right.split(",")))
    return (l, r)

def parse(file=filename):
    return [to_coords(*line.strip().split("~"))
            for line in open(file).readlines()]

def sort_by_z(bricks):
    return sorted(bricks, key=lambda brick: min(brick[0][2], brick[1][2]))

def get_max_z(bricks):
    if not bricks:
        return 1
    return max(max(brick[0][2], brick[1][2]) for brick in bricks)

def has_collision(x_min, x_max, y_min, y_max, z, bricks):
    for x in range(x_min, x_max + 1):
        for y in range(y_min, y_max + 1):
            for (lx, ly, lz), (rx, ry, rz) in bricks:
                if (lx <= x <= rx and ly <= y <= ry and lz <= z <= rz):
                    return True
    return False

def drop(bricks):
    settled = []
    queue = deque(sort_by_z(bricks))
    blocks_moved = 0

    while queue:
        (lx, ly, lz), (rx, ry, rz) = queue.popleft()

        x_min, x_max = min(lx, rx), max(lx, rx)
        y_min, y_max = min(ly, ry), max(ly, ry)
        z_min        = min(lz, rz)

        z = get_max_z(settled) + 1
        while z > 1:
            if has_collision(x_min, x_max, y_min, y_max, z - 1, settled):
                break
            z -= 1

        z_diff = z_min - z
        assert z_diff >= 0, "brick would need to move upwards"
        settled.append(((lx, ly, lz - z_diff), (rx, ry, rz - z_diff)))
        if z_diff > 0:
            blocks_moved += 1

    return settled, blocks_moved

# BUG: fast, bug wrong (returns more disintegratables than there are)
# def can_be_disintegrated(brick, other_bricks):
#     (lx, ly, lz), (rx, ry, rz) = brick
#
#     x_min, x_max = min(lx, rx), max(lx, rx)
#     y_min, y_max = min(ly, ry), max(ly, ry)
#     z = min(lz, rz) + 1
#
#     above = [b for b in other_bricks
#              if has_collision(x_min, x_max, y_min, y_max, z, [b])]
#
#     debug_bricks = [brick] + above
#
#     # for each brick above, check if we are the *only* supporting brick below
#     for brick_above in above:
#         (alx, aly, alz), (arx, ary, arz) = brick_above
#
#         ax_min, ax_max = min(alx, arx), max(alx, arx)
#         ay_min, ay_max = min(aly, ary), max(aly, ary)
#         az = min(alz, arz) - 1
#
#         below = [b for b in other_bricks
#                  if has_collision(ax_min, ax_max, ay_min, ay_max, az, [b])]
#         debug_bricks += below
#         if not below:
#             return (False, debug_bricks)
#
#     return (True, debug_bricks)

def draw_windows(bricks, disintegratables=[], start=1):
    window_size = 3 if is_sample else 8

    step_size = window_size - 1 # to get one plane overlap

    for z_min in range(start, get_max_z(bricks) + 1, step_size):
        draw_voxels(bricks, disintegratables=disintegratables,
                    z_min=z_min, z_max=z_min + step_size)

def draw_voxels(bricks, disintegratables=[], z_min=1, z_max=None):
    import numpy as np
    import matplotlib.pyplot as plt
    import matplotlib
    from matplotlib.colors import hex2color, ColorConverter
    matplotlib.use('TkAgg')

    if z_max is None:
        z_max = get_max_z(bricks)

    WIDTH  = max(max(lx, rx) for (lx, _, _), (rx, _, _) in bricks) + 1
    DEPTH  = max(max(ly, ry) for (_, ly, _), (_, ry, _) in bricks) + 1
    HEIGHT = max(max(lz, rz) for (_, _, lz), (_, _, rz) in bricks) + 1

    if z_min is not None and z_max is not None:
        HEIGHT = z_max - z_min + 1

    brick_colors = ['#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd',
                    '#8c564b', '#e377c2', '#bcbd22', '#17becf']

    greys = ['#808080', '#a6a6a6', '#cccccc', '#f2f2f2']

    colors = np.empty((WIDTH, DEPTH, HEIGHT, 4), dtype=float)
    voxels = np.zeros((WIDTH, DEPTH, HEIGHT),    dtype=bool)

    for i, brick in enumerate(bricks, start=1):
        hex_color = brick_colors[i % len(brick_colors)]
        alpha = 0.8
        # NOTE: both variants look nice, depends on what you want to see better
        # if disintegratables and brick not in disintegratables:
        if disintegratables and brick in disintegratables:
            hex_color = greys[i % len(greys)]
            alpha = 0.2
        rgb_color = np.append(hex2color(hex_color), alpha)

        for (lx, ly, lz), (rx, ry, rz) in [brick]:
            if z_min is not None and z_max is not None:
                lz -= z_min
                rz -= z_min

            if lx == rx and ly == ry:
                # pillar (z going up and down)
                minz = max(min(lz, rz), 0)
                maxz = min(max(lz, rz), HEIGHT - 1)
                if minz > maxz:
                    continue
                colors[lx, ly, minz:maxz + 1] = rgb_color
                voxels[lx, ly, minz:maxz + 1] = True

            elif lx == rx and lz == rz and 0 <= lz <= HEIGHT - 1:
                # horizontal plane (y going left and right)
                miny, maxy = min(ly, ry), max(ly, ry)
                colors[lx, miny:maxy + 1, lz] = rgb_color
                voxels[lx, miny:maxy + 1, lz] = True

            elif ly == ry and lz == rz and 0 <= lz <= HEIGHT - 1:
                # horizontal plane (x going left and right)
                minx, maxx = min(lx, rx), max(lx, rx)
                colors[minx:maxx + 1, ly, lz] = rgb_color
                voxels[minx:maxx + 1, ly, lz] = True

    ax = plt.figure().add_subplot(projection='3d')
    border_color = ColorConverter.to_rgba('black', alpha=0.05)
    ax.voxels(voxels, facecolors=colors, edgecolor=border_color)

    ax.set_box_aspect((WIDTH, DEPTH, HEIGHT)) # force cubes to be cubes

    ax.set_xticks(range(0,  WIDTH + 1), labels=range(0,  WIDTH + 1))
    ax.set_yticks(range(0, DEPTH + 1), labels=range(0, DEPTH + 1))
    ax.set_zticks(range(z_min - z_min, z_max + 2 - z_min),
                  labels=range(z_min, z_max + 2))

    ax.set_xlabel('x')
    ax.set_ylabel('y')
    ax.set_zlabel('z')

    plt.show()

class TestDay22(unittest.TestCase):
    def test_parse_sample(self):
        bricks = parse("sample.txt")
        self.assertEqual(set(bricks), set([
            ((1, 0, 1), (1, 2, 1)),
            ((0, 0, 2), (2, 0, 2)),
            ((0, 2, 3), (2, 2, 3)),
            ((0, 0, 4), (0, 2, 4)),
            ((2, 0, 5), (2, 2, 5)),
            ((0, 1, 6), (2, 1, 6)),
            ((1, 1, 8), (1, 1, 9)),
            ]))

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
    unittest.main(exit=False)
    print()

    bricks = parse()

    if visualize and is_sample:
        draw_voxels(bricks)

    bricks, _ = drop(bricks)

    if visualize:
        if is_sample:
            draw_voxels(bricks)
        else:
            draw_windows(bricks)

    disintegratable_bricks = 0
    brick_falls = 0

    for brick in tqdm(bricks):
        other_bricks = [b for b in bricks if b != brick]
        other_bricks_dropped, fallen_bricks = drop(other_bricks)
        brick_falls += fallen_bricks
        if not fallen_bricks:
            disintegratable_bricks += 1

    part1 = disintegratable_bricks
    part2 = brick_falls

    check(1, part1, 5 if is_sample else 501)
    check(2, part2, 7 if is_sample else 80948)
