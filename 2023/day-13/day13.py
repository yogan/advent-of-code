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

def find_reflection_index(block, already_found_idx=None):
    for i in range(1, len(block)):
        if i == already_found_idx:
            continue
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

def calc(blocks):
    part1 = 0
    part2 = 0

    for block in blocks:
        row1 = find_reflection_index(block)
        col1 = find_reflection_index(list(zip(*block)))
        part1 += col1 + 100 * row1

        W, H = len(block[0]), len(block)
        for i in range(0, H):
            for j in range(0, W):
                block[i][j] = "." if block[i][j] == "#" else "#"   # fix smudge
                row2 = find_reflection_index(block, row1)
                col2 = find_reflection_index(list(zip(*block)), col1)
                block[i][j] = "." if block[i][j] == "#" else "#"  # revert back
                if row2 > 0 or col2 > 0:
                    part2 += col2 + 100 * row2
                    break
            else:
                continue
            break

    return part1, part2

def print_and_assert(part, expected, actual):
    print(f"Part {part}: {actual}{' (sample)' if is_sample else ''}")
    assert actual == expected, f"{part} was {actual}, expected {expected}"

if __name__ == '__main__':
    blocks = parse()
    part1, part2 = calc(blocks)

    print_and_assert(1, 405 if is_sample else 33195, part1)
    print_and_assert(2, 400 if is_sample else 31836, part2)
