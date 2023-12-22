import sys, unittest

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
is_sample = filename == "sample.txt"

def to_coords(left, right):
    l = tuple(map(int, left.split(",")))
    r = tuple(map(int, right.split(",")))
    return (l, r)

def parse(file=filename):
    return [to_coords(*line.strip().split("~"))
            for line in open(file).readlines()]

def draw_voxels(blocks):
    import numpy as np
    import matplotlib.pyplot as plt
    import matplotlib
    from matplotlib.colors import hex2color
    matplotlib.use('TkAgg')

    WIDTH  = max(max(lx, rx) for (lx, _, _), (rx, _, _) in blocks) + 1
    HEIGHT = max(max(ly, ry) for (_, ly, _), (_, ry, _) in blocks) + 1
    DEPTH  = max(max(lz, rz) for (_, _, lz), (_, _, rz) in blocks) + 1

    block_colors = ['#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd',
                    '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf']

    colors = np.empty((WIDTH, HEIGHT, DEPTH, 4), dtype=float)
    voxels = np.zeros((WIDTH, HEIGHT, DEPTH), dtype=bool)

    for i, block in enumerate(blocks, start=1):
        hex_color = block_colors[i % len(block_colors)]
        rgb_color = np.append(hex2color(hex_color), 0.8)
        for (lx, ly, lz), (rx, ry, rz) in [block]:
            if lx == rx and ly == ry:
                minz, maxz = min(lz, rz), max(lz, rz)
                colors[lx, ly, minz:maxz + 1] = rgb_color
                voxels[lx, ly, minz:maxz + 1] = True
            elif lx == rx and lz == rz:
                miny, maxy = min(ly, ry), max(ly, ry)
                colors[lx, miny:maxy + 1, lz] = rgb_color
                voxels[lx, miny:maxy + 1, lz] = True
            elif ly == ry and lz == rz:
                minx, maxx = min(lx, rx), max(lx, rx)
                colors[minx:maxx + 1, ly, lz] = rgb_color
                voxels[minx:maxx + 1, ly, lz] = True
            else:
                assert False, "Invalid block"

    ax = plt.figure().add_subplot(projection='3d')
    ax.voxels(voxels, facecolors=colors, edgecolor='k')

    ax.set_box_aspect((WIDTH, HEIGHT, DEPTH)) # force cubes to be cubes
    ax.set_xticklabels([])
    ax.set_yticklabels([])
    ax.set_zticklabels([])

    plt.show()

class TestDay22(unittest.TestCase):
    def test_parse_sample(self):
        blocks = parse("sample.txt")
        self.assertEqual(set(blocks), set([
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

    blocks = parse()

    if visualize:
        draw_voxels(blocks)

    part1 = None
    part2 = None

    check(1, part1, None if is_sample else None)
    check(2, part2)
