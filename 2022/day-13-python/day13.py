import sys, math
from functools import cmp_to_key

def parse_input(filename):
    lines = open(filename).read().split("\n\n")
    return [eval(line.strip().replace("\n", ", ")) for line in lines]

def compare(left, right):
    match (left, right):
        case int(), int():
            return left - right
        case int(), list():
            return compare([left], right)
        case list(), int():
            return compare(left, [right])
        case list(), list():
            for l, r in zip(left, right):
                diff = compare(l, r)
                if (diff != 0):
                    return diff
            # all zipped were equal, but we could have more in left or right
            return len(left) - len(right)

def get_in_order(packets):
    right_order = []
    for i, pair in enumerate(packets):
        if (compare(pair[0], pair[1]) < 0):
            right_order.append(i + 1)
    return right_order

def find_dividers(packets):
    dividers = [[[2]], [[6]]]
    packets = [packet for pair in packets for packet in pair] + dividers
    sorted_packets = sorted(packets, key=cmp_to_key(compare))
    return [sorted_packets.index(p) + 1 for p in dividers]

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

decoder_key = math.prod(find_dividers(input))
assert(decoder_key == 25792)
print("Part 2:", decoder_key)
