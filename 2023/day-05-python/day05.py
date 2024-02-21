import sys, unittest
from typing import NamedTuple

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they scare the unittest module
is_sample = filename == "sample.txt"

class Range(NamedTuple):
    start: int
    end:   int

class Mapping(NamedTuple):
    src_start: int
    src_end:   int
    offset:    int

def parse_map(map_str_with_newlines):
    lines = map_str_with_newlines.split("\n")
    mappings = []
    for line in lines[1:]:
        dst_start, src_start, l = [int(x) for x in line.split(" ")]
        offset = dst_start - src_start
        mappings.append(Mapping(src_start, src_start + l - 1, offset))
    return mappings

def parse(is_part_1):
    with open(filename) as f:
        blocks = f.read()[:-1].split("\n\n")
        assert len(blocks) == 8

    seeds = [int(x) for x in blocks[0].split(": ")[1].split(" ")]
    seed_ranges = []
    if is_part_1:
        for seed in seeds:
            seed_ranges.append(Range(seed, seed))
    else:
        for i in range(0, len(seeds), 2):
            seed_ranges.append(Range(seeds[i], seeds[i] + seeds[i+1] - 1))

    maps = [parse_map(x) for x in blocks[1:]]

    return [seed_ranges, maps]

def find_intersecting_mappings(seed_range, map):
    s_start, s_end = seed_range
    return [m for m in map if m.src_start <= s_end and m.src_end >= s_start]

def fill_mapping_gaps(seed_range, mappings):
    filled_mappings = []

    s_cur = seed_range.start
    while s_cur <= seed_range.end:
        maybe_mapping = [m for m in mappings if m.src_start <= s_cur and m.src_end >= s_cur]
        if maybe_mapping:
            mapping = maybe_mapping[0]
            filled_mappings.append(mapping)
            s_cur = mapping.src_end + 1
        else:
            mapping_starts = [m.src_start for m in mappings if m.src_start > s_cur]
            if mapping_starts:
                s_end = min(mapping_starts) - 1
            else:
                s_end = seed_range.end
            filled_mappings.append(Mapping(s_cur, s_end, 0))
            s_cur = s_end + 1

    return filled_mappings

def apply_mappings(seed_range, map):
    intersecting_mappings = find_intersecting_mappings(seed_range, map)
    if not intersecting_mappings:
        return [seed_range]

    filled_mappings = fill_mapping_gaps(seed_range, intersecting_mappings)
    mapped_seed_ranges = []

    s_cur = seed_range.start
    while s_cur <= seed_range.end:
        mappings = [m for m in filled_mappings if m.src_start <= s_cur and m.src_end >= s_cur]
        assert len(mappings) == 1
        mapping = mappings[0]

        s_end = min(mapping.src_end, seed_range.end)
        mapped_seed_ranges.append(Range(s_cur + mapping.offset, s_end + mapping.offset))
        s_cur = s_end + 1

    return mapped_seed_ranges

def apply_map(seed_ranges, map):
    mapped_seed_ranges = []

    for seed_range in seed_ranges:
        ranges = apply_mappings(seed_range, map)
        for r in ranges:
            mapped_seed_ranges.append(r)

    return mapped_seed_ranges

def find_locations(seed_ranges, maps):
    for map in maps:
        seed_ranges = apply_map(seed_ranges, map)

    return min(seed_ranges, key=lambda x: x[0])[0]

class TestDay05(unittest.TestCase):
    def test_find_intersecting_mappings_first_of_two(self):
        map = [Mapping(50, 97, 2), Mapping(98, 99, -48)]
        seed_range = Range(49, 51)
        self.assertEqual(find_intersecting_mappings(seed_range, map),
                         [Mapping(50, 97, 2)])

    def test_find_intersecting_mappings_second_of_two(self):
        map = [Mapping(50, 97, 2), Mapping(98, 99, -48)]
        seed_range = Range(98, 98)
        self.assertEqual(find_intersecting_mappings(seed_range, map),
                         [Mapping(98, 99, -48)])

    def test_find_intersecting_mappings_two_of_two(self):
        map = [Mapping(50, 97, 2), Mapping(98, 99, -48)]
        seed_range = Range(97, 98)
        self.assertEqual(find_intersecting_mappings(seed_range, map),
                         [Mapping(50, 97, 2), Mapping(98, 99, -48)])

    def test_find_intersecting_mappings_none_of_two(self):
        map = [Mapping(50, 97, 2), Mapping(98, 99, -48)]
        seed_range = Range(40, 49)
        self.assertEqual(find_intersecting_mappings(seed_range, map), [])

    def test_find_intersecting_mappings_two_of_multiple_with_holes(self):
        map = [Mapping(1, 7, 1), Mapping(10, 12, 2), Mapping(15, 20, 3)]
        seed_range = Range(5, 11)
        self.assertEqual(find_intersecting_mappings(seed_range, map),
                         [Mapping(1, 7, 1), Mapping(10, 12, 2)])

    def test_find_intersecting_mappings_all_of_multiple_with_holes(self):
        map = [Mapping(1, 7, 1), Mapping(10, 12, 2), Mapping(15, 20, 3)]
        seed_range = Range(1, 999)
        self.assertEqual(find_intersecting_mappings(seed_range, map),
                         [Mapping(1, 7, 1), Mapping(10, 12, 2), Mapping(15, 20, 3)])

    def test_find_intersecting_mappings_none_of_multiple_with_holes(self):
        map = [Mapping(1, 7, 1), Mapping(10, 12, 2), Mapping(15, 20, 3)]
        seed_range = Range(13, 14)
        self.assertEqual(find_intersecting_mappings(seed_range, map), [])

    def test_fill_mapping_gaps_no_mappings(self):
        seed_range = Range(1, 10)
        mappings = []
        filled_mappings = fill_mapping_gaps(seed_range, mappings)
        self.assertEqual(filled_mappings, [Mapping(1, 10, 0)])

    def test_fill_mapping_gaps_left_missing(self):
        seed_range = Range(1, 10)
        mappings = [Mapping(2, 10, 1)]
        filled_mappings = fill_mapping_gaps(seed_range, mappings)
        self.assertEqual(filled_mappings, [Mapping(1, 1, 0), Mapping(2, 10, 1)])

    def test_fill_mapping_gaps_right_missing(self):
        seed_range = Range(4, 12)
        mappings = [Mapping(1, 9, 5)]
        filled_mappings = fill_mapping_gaps(seed_range, mappings)
        self.assertEqual(filled_mappings, [Mapping(1, 9, 5), Mapping(10, 12, 0)])

    def test_fill_mapping_gaps_middle_missing(self):
        seed_range = Range(1, 20)
        mappings = [Mapping(1, 9, 5), Mapping(15, 20, 3)]
        filled_mappings = fill_mapping_gaps(seed_range, mappings)
        self.assertEqual(filled_mappings,
                         [Mapping(1, 9, 5), Mapping(10, 14, 0), Mapping(15, 20, 3)])

    def test_apply_mappings_single_seed_not_shifted(self):
        map = [Mapping(50, 97, 2), Mapping(98, 99, -48)]
        seed_range = Range(49, 49)
        mapped_ranges = apply_mappings(seed_range, map)
        self.assertEqual(mapped_ranges, [Range(49, 49)])

    def test_apply_mappings_single_seed_shifted(self):
        map = [Mapping(50, 97, 2), Mapping(98, 99, -48)]
        seed_range = Range(50, 50)
        mapped_ranges = apply_mappings(seed_range, map)
        self.assertEqual(mapped_ranges, [Range(52, 52)])

if __name__ == '__main__':
    unittest.main(exit=False)
    print()

    seeds1, maps = parse(is_part_1=True)
    seeds2, _    = parse(is_part_1=False)

    res1 = find_locations(seeds1, maps)
    assert res1 == (35 if is_sample else 227653707)
    print(f"Part 1: {res1}{' (sample)' if is_sample else ''}")

    res2 = find_locations(seeds2, maps)
    assert res2 == (46 if is_sample else 78775051)
    print(f"Part 2: {res2}{' (sample)' if is_sample else ''}")
