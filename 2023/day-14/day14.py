import sys, unittest

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they scare the unittest module
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

def tilt_north(lines):
    # rotation stuff is a bit expensive, but shifting east is so easy to code…
    lines_east = rotate_clockwise(lines)
    lines_east_tilted = tilt_east(lines_east)
    return rotate_counter_clockwise(lines_east_tilted)

def score(lines):
    sum = 0
    H = len(lines)
    for i, line in enumerate(lines):
        factor = H - i
        rounded_rocks = [rock for rock in line if rock == "O"]
        sum += len(rounded_rocks) * factor
    return sum

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
    assert actual == expected, f"{part} was {actual}, expected {expected}"

if __name__ == '__main__':
    unittest.main(exit=False)
    print()

    map = parse()
    map_tilted_north = tilt_north(map)
    part1 = score(map_tilted_north)

    print_and_assert(1, 136 if is_sample else 109345, part1)
    # print_and_assert(2, 21756 if is_sample else 4978, part2(lines))
