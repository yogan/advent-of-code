import re
import unittest
from input import read_and_solve


def parse_input(lines):
    steps = []
    for line in lines:
        on_off, coords = line.split(" x=")
        ranges = re.split(",[yz]=", coords)
        coord_pairs = [tuple(map(int, r.split(".."))) for r in ranges]
        steps.append((on_off == "on", *coord_pairs))
    return steps


def part1(lines):
    steps = parse_input(lines)
    active_cubes = set()

    for on, (x_start, x_end), (y_start, y_end), (z_start, z_end) in steps:
        for x in range(x_start, x_end + 1):
            if abs(x) > 50:
                continue
            for y in range(y_start, y_end + 1):
                if abs(y) > 50:
                    continue
                for z in range(z_start, z_end + 1):
                    if abs(z) > 50:
                        continue
                    if on:
                        active_cubes.add((x, y, z))
                    else:
                        active_cubes.discard((x, y, z))

    return len(active_cubes)


def part2(lines):
    return 0


class TestDay22(unittest.TestCase):

    small_sample = [
        "on x=10..12,y=10..12,z=10..12",
        "on x=11..13,y=11..13,z=11..13",
        "off x=9..11,y=9..11,z=9..11",
        "on x=10..10,y=10..10,z=10..10",
    ]

    sample = [
        "on x=-20..26,y=-36..17,z=-47..7",
        "on x=-20..33,y=-21..23,z=-26..28",
        "on x=-22..28,y=-29..23,z=-38..16",
        "on x=-46..7,y=-6..46,z=-50..-1",
        "on x=-49..1,y=-3..46,z=-24..28",
        "on x=2..47,y=-22..22,z=-23..27",
        "on x=-27..23,y=-28..26,z=-21..29",
        "on x=-39..5,y=-6..47,z=-3..44",
        "on x=-30..21,y=-8..43,z=-13..34",
        "on x=-22..26,y=-27..20,z=-29..19",
        "off x=-48..-32,y=26..41,z=-47..-37",
        "on x=-12..35,y=6..50,z=-50..-2",
        "off x=-48..-32,y=-32..-16,z=-15..-5",
        "on x=-18..26,y=-33..15,z=-7..46",
        "off x=-40..-22,y=-38..-28,z=23..41",
        "on x=-16..35,y=-41..10,z=-47..6",
        "off x=-32..-23,y=11..30,z=-14..3",
        "on x=-49..-5,y=-3..45,z=-29..18",
        "off x=18..30,y=-20..-8,z=-3..13",
        "on x=-41..9,y=-7..43,z=-33..15",
        "on x=-54112..-39298,y=-85059..-49293,z=-27449..7877",
        "on x=967..23432,y=45373..81175,z=27513..53682",
    ]

    def test_parse_input(self):
        steps = parse_input(self.small_sample)
        self.assertEqual(steps, [
            (True,  (10, 12), (10, 12), (10, 12)),
            (True,  (11, 13), (11, 13), (11, 13)),
            (False,  (9, 11),  (9, 11),  (9, 11)),
            (True,  (10, 10), (10, 10), (10, 10)),
        ])

    def test_part_1_sample(self):
        self.assertEqual(part1(self.sample), 590784)

    # def test_part_2_sample(self):
    #     self.assertEqual(part2(self.sample), TODO)


if __name__ == '__main__':
    unittest.main(exit=False)
    read_and_solve(__file__, part1, part2)
