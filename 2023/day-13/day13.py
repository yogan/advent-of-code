import sys

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
is_sample = filename == "sample.txt"

def parse():
    with open(filename) as f:
        return [[list(line) for line in block.strip().split("\n")]
                for block in f.read().split("\n\n")]

def find_reflection_index(block):
    for i in range(1, len(block)):
        distance_to_top = i
        distance_to_bottom = len(block) - i
        distance = min(distance_to_top, distance_to_bottom)
        ok = True
        for j in range(0, distance):
            lower = i - j - 1
            upper = i + j
            assert lower >= 0
            assert upper < len(block)
            assert lower < upper, f"{lower} >= {upper}"
            if block[lower] != block[upper]:
                ok = False
                continue
        if ok:
            return i

    return 0

def part1(blocks):
    sum = 0
    for block in blocks:
        row = find_reflection_index(block)
        col = find_reflection_index(list(zip(*block)))
        sum += col + 100 * row
    return sum

def print_and_assert(part, expected, actual):
    print(f"Part {part}: {actual}{' (sample)' if is_sample else ''}")
    assert actual == expected, f"{part} was {actual}, expected {expected}"

if __name__ == '__main__':
    blocks = parse()

    print_and_assert(1, 405 if is_sample else 33195, part1(blocks))
    # print_and_assert(2, 21756 if is_sample else 4978, part2(lines))
