import sys

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
is_sample = filename == "sample.txt"

def parse():
    return open(filename).read().strip().split(",")

def hash(string):
    value = 0
    for char in string:
        value += ord(char)
        value *= 17
        value %= 256
    return value

def fill_boxes(input, hashes):
    boxes = [list() for _ in range(256)]

    for step in input:
        if "-" in step:
            label, focal_length = step[:-1], None
        else:
            label, focal_length = step.split("=")

        box_idx = hash(label)

        if focal_length:
            labels = [label for label, _ in boxes[box_idx]]
            if label in labels:
                lens_idx = labels.index(label)
                boxes[box_idx][lens_idx] = (label, focal_length)
            else:
                boxes[box_idx].append((label, focal_length))
        else:
            boxes[box_idx] = [(l, f) for l, f in boxes[box_idx] if l != label]

    return boxes

def focusing_power(boxes):
    sum = 0
    for box_idx, box in enumerate(boxes, 1):
        for lens_idx, (_, focal_length) in enumerate(box, 1):
            sum += box_idx * lens_idx * int(focal_length)
    return sum

def print_and_assert(part, expected, actual):
    print(f"Part {part}: {actual}{' (sample)' if is_sample else ''}")
    assert actual == expected, f"Part {part} was {actual}, expected {expected}"

if __name__ == '__main__':
    sequence = parse()

    hashes = [hash(step) for step in sequence]
    part1 = sum(hashes)

    boxes = fill_boxes(sequence, hashes)
    part2 = focusing_power(boxes)

    print_and_assert(1, 1320 if is_sample else 514639, part1)
    print_and_assert(2,  145 if is_sample else 279470, part2)
