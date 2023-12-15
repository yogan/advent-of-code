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

def fill_boxes(input, hashes):
    boxes = [list() for _ in range(256)]

    for step in input:
        if step[-1] == "-":
            label, focal_length = step[:-1], None
        else:
            label, focal_length = step.split("=")

        box_idx = hash(label)

        if focal_length:
            labels = [pair[0] for pair in boxes[box_idx]]
            if label in labels:
                idx = labels.index(label)
                boxes[box_idx][idx] = (label, focal_length)
            else:
                boxes[box_idx].append((label, focal_length))
        else:
            boxes[box_idx] = [x for x in boxes[box_idx] if x[0] != label]

    return boxes

def focusing_power(boxes):
    sum = 0

    for box_idx, box in enumerate(boxes):
        for lens_idx, lens in enumerate(box):
            sum += (box_idx + 1) * (lens_idx + 1) * int(lens[1])

    return sum

def print_and_assert(part, expected, actual):
    print(f"Part {part}: {actual}{' (sample)' if is_sample else ''}")
    assert actual == expected, f"Part {part} was {actual}, expected {expected}"

if __name__ == '__main__':
    input = parse()

    hashes = [hash(str) for str in input]
    part1 = sum(hashes)

    boxes = fill_boxes(input, hashes)
    part2 = focusing_power(boxes)

    print_and_assert(1, 1320 if is_sample else 514639, part1)
    print_and_assert(2,  145 if is_sample else 279470, part2)
