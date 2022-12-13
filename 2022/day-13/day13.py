import sys
from functools import cmp_to_key

def parse_input(filename):
    lines = open(filename).read().split("\n\n")
    return [eval(line.strip().replace("\n", ", ")) for line in lines]

def compare(left, right):
    match (left, right):
        case int(), int():
            return left - right
        case list(), list():
            for l, r in zip(left, right):
                diff = compare(l, r)
                if (diff != 0):
                    return diff
            # all zipped were the same, but we could have more in left or right
            return len(left) - len(right)
        case int(), list():
            return compare([left], right)
        case list(), int():
            return compare(left, [right])

def get_in_order(packets):
    right_order = []
    for i, pair in enumerate(packets):
        if (compare(pair[0], pair[1]) < 0):
            right_order.append(i + 1)
    return right_order

def to_part2_format(pairs):
    packets = [packet for pair in pairs for packet in pair]
    packets.append([[2]])
    packets.append([[6]])
    return packets

def find_dividers(packets):
    sorted_packets = sorted(to_part2_format(packets), key=cmp_to_key(compare))
    return [sorted_packets.index(p) + 1 for p in [[[2]], [[6]]]]

input = parse_input("day13.in")
sample = parse_input("day13.sample")

# Part 1
right_order_sample = get_in_order(sample)
assert(right_order_sample == [1, 2, 4, 6])

part1 = sum(get_in_order(input))
assert(part1 == 5208)
print("Part 1:", part1)

# Part 2
assert(find_dividers(sample) == [10, 14])

dividers = find_dividers(input)
decoder_key = dividers[0] * dividers[1]
assert(decoder_key == 25792)
print("Part 2:", decoder_key)
