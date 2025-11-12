import sys, unittest

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
is_sample = filename == "sample.txt"

def parse():
    with open(filename) as f:
        return [list(x.strip()) for x in f.readlines()]

def rotate_clockwise(lines):
    return list(zip(*lines[::-1]))

def rotate_counter_clockwise(lines):
    return list(zip(*lines))[::-1]

def tilt_line_east(line):
    new_line = []

    dist = 0
    rounded = 0
    last_cube_idx = -1

    for i in range(len(line)):
        if line[i] == "#":
            dots = i - rounded - last_cube_idx - 1
            for d in range(dist):
                new_line.append("." if d < dots else "O")
            new_line.append("#")

            dist = 0
            rounded = 0
            last_cube_idx = i
            continue

        dist += 1
        if line[i] == "O":
            rounded += 1

    dots = len(line) - rounded - last_cube_idx - 1
    for d in range(dist):
        new_line.append("." if d < dots else "O")

    return new_line

def tilt_east(lines):
    return [tilt_line_east(line) for line in lines]

def tilt_west(lines):
    return [list(reversed(tilt_line_east(list(reversed(line)))))
            for line in lines]

def tilt_north(lines):
    # rotation stuff is a bit expensive, but shifting east is so easy to code…
    lines_east = rotate_clockwise(lines)
    lines_east_tilted = tilt_east(lines_east)
    return rotate_counter_clockwise(lines_east_tilted)

def tilt_south(lines):
    lines_west = rotate_counter_clockwise(lines)
    lines_west_tilted = tilt_east(lines_west)
    return rotate_clockwise(lines_west_tilted)

def total_load(lines):
    load = 0
    H = len(lines)
    for i, line in enumerate(lines):
        factor = H - i
        rounded_rocks = [rock for rock in line if rock == "O"]
        load += len(rounded_rocks) * factor
    return load

def cycle(map):
    map = tilt_north(map)
    map = tilt_west(map)
    map = tilt_south(map)
    return tilt_east(map)

def do_cycles(map):
    cache = {}
    cycle_offset = 0

    LIMIT = 1_000_000_000

    for i in range(LIMIT):
        key = "".join(["".join(line) for line in map])
        if key in cache:
            map, cycle_offset = cache[key]
            break
        else:
            map = cycle(map)
            cache[key] = (map, i)

    cycle_length = i - cycle_offset
    left = (LIMIT - cycle_offset - 1) % cycle_length

    for i in range(left):
        map = cycle(map)

    return map

class TestDay14(unittest.TestCase):
    def test_tilt_line_east_sample(self):
        self.assertEqual(tilt_line_east(list("OO.O.O..##")), list("....OOOO##"))
        self.assertEqual(tilt_line_east(list("...OO....O")), list(".......OOO"))
        self.assertEqual(tilt_line_east(list(".O...#O..O")), list("....O#..OO"))
        self.assertEqual(tilt_line_east(list(".O.#......")), list("..O#......"))
        self.assertEqual(tilt_line_east(list(".#.O......")), list(".#.......O"))
        self.assertEqual(tilt_line_east(list("#.#..O#.##")), list("#.#..O#.##"))
        self.assertEqual(tilt_line_east(list("..#...O.#.")), list("..#....O#."))
        self.assertEqual(tilt_line_east(list("....O#.O#.")), list("....O#.O#."))
        self.assertEqual(tilt_line_east(list("....#.....")), list("....#....."))
        self.assertEqual(tilt_line_east(list(".#.O.#O...")), list(".#..O#...O"))

    def test_tilt_north_sample(self):
        sample = [
                tuple("O....#...."),
                tuple("O.OO#....#"),
                tuple(".....##..."),
                tuple("OO.#O....O"),
                tuple(".O.....O#."),
                tuple("O.#..O.#.#"),
                tuple("..O..#O..O"),
                tuple(".......O.."),
                tuple("#....###.."),
                tuple("#OO..#...."),
                ]
        expected = [
                tuple("OOOO.#.O.."),
                tuple("OO..#....#"),
                tuple("OO..O##..O"),
                tuple("O..#.OO..."),
                tuple("........#."),
                tuple("..#....#.#"),
                tuple("..O..#.O.O"),
                tuple("..O......."),
                tuple("#....###.."),
                tuple("#....#...."),
                ]
        self.assertEqual(tilt_north(sample), expected)

def print_and_assert(part, expected, actual):
    print(f"Part {part}: {actual}{' (sample)' if is_sample else ''}")
    assert actual == expected, f"Part {part} was {actual}, expected {expected}"

if __name__ == '__main__':
    unittest.main(argv=sys.argv[:1], exit=False)
    print()

    map = parse()
    part1 = total_load(tilt_north(map))
    part2 = total_load(do_cycles(map))

    print_and_assert(1, 136 if is_sample else 109345, part1)
    print_and_assert(2, 64 if is_sample else 112452, part2)
