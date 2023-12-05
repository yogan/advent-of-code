import sys

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
is_sample = filename == "sample.txt"

def parse_map(map_str_with_newlines):
    lines = map_str_with_newlines.split("\n")
    mappings = []
    for line in lines[1:]:
        mappings.append([int(x) for x in line.split(" ")])
    return mappings

def parse():
    with open(filename) as f:
        blocks = f.read()[:-1].split("\n\n")
        assert len(blocks) == 8
        seeds = [int(x) for x in blocks[0].split(": ")[1].split(" ")]
        maps = [parse_map(x) for x in blocks[1:]]

    return [seeds, maps]

def part1(seeds, maps):
    locations = []

    for seed in seeds:
        for map in maps:
            for mapping in map:
                dst_start, src_start, l = mapping
                offset = dst_start - src_start
                if seed >= src_start and seed < src_start + l:
                    seed = seed + offset
                    break
        locations.append(seed)

    # print("locations", locations)
    return min(locations)

if __name__ == '__main__':
    seeds, maps = parse()

    res1 = part1(seeds, maps)
    assert res1 == (35 if is_sample else 227653707)
    print(f"Part 1: {res1}{' (sample)' if is_sample else ''}")

    # res2 = part2(lines, part_numbers)
    # assert res2 == (467835 if is_sample else 72553319)
    # print(f"Part 2: {res2}{' (sample)' if is_sample else ''}")
