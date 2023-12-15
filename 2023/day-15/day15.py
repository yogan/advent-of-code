import sys

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
is_sample = filename == "sample.txt"

def parse():
    return open(filename).read().strip().split(",")

def hash(str):
    cur = 0
    for c in str:
        cur += ord(c)
        cur *= 17
        cur &=0xFF
    return cur

def print_and_assert(part, expected, actual):
    print(f"Part {part}: {actual}{' (sample)' if is_sample else ''}")
    assert actual == expected, f"Part {part} was {actual}, expected {expected}"

if __name__ == '__main__':
    hashes = [hash(str) for str in parse()]
    part1 = sum(hashes)

    print_and_assert(1, 1320 if is_sample else 514639, part1)
    # print_and_assert(2, None if is_sample else None, part2(lines))
