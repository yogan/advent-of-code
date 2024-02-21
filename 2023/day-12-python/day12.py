import sys

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
is_sample = filename == "sample.txt"

def parse(factor=1):
    with open(filename) as f:
        lines = [line.strip().split() for line in f.readlines()]

    return [["?".join([pattern] * factor),
             tuple(map(int, groups.split(","))) * factor]
            for pattern, groups in lines]

def count(pattern, groups, cache={}):
    if pattern == "":
        # reached end of pattern; check if groups are all used up
        return 1 if groups == () else 0

    if groups == ():
        # reached end of groups; remaining pattern may not contain any #s
        return 0 if "#" in pattern else 1

    if (pattern, groups) in cache:
        return cache[(pattern, groups)]

    sum = 0

    if pattern[0] in ".?":
        sum += count(pattern[1:], groups)

    if pattern[0] in "#?":
        cur_group = groups[0]
        if (cur_group <= len(pattern)
            and "." not in pattern[:cur_group]
            and (cur_group == len(pattern) or pattern[cur_group] != "#")):
            start_of_next_group = cur_group + 1 # offset for . as separator
            sum += count(pattern[start_of_next_group:], groups[1:])

    cache[(pattern, groups)] = sum
    return sum

def print_and_assert(part, expected, actual):
    print(f"Part {part}: {actual}{' (sample)' if is_sample else ''}")
    assert actual == expected, f"{part} was {actual}, expected {expected}"

if __name__ == '__main__':
    part1 = sum([count(pattern, groups) for pattern, groups in parse()])
    part2 = sum([count(pattern, groups) for pattern, groups in parse(5)])

    print_and_assert(1, 21 if is_sample else 7169, part1)
    print_and_assert(2, 525152 if is_sample else 1738259948652, part2)
