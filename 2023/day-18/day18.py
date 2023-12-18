import sys, math

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
is_sample = filename == "sample.txt"

def parse():
    return [(dir, int(steps), color[2:-1]) for dir, steps, color in
            [line.split() for line in open(filename).readlines()]]

def dig(plan):
    min_x, min_y = math.inf, math.inf
    max_x, max_y = 0, 0
    pos = (0, 0)
    border = set()
    border.add(pos)

    for dir, steps, _ in plan:
        for _ in range(steps):
            if dir == "U":
                pos = (pos[0], pos[1] - 1)
            elif dir == "D":
                pos = (pos[0], pos[1] + 1)
            elif dir == "R":
                pos = (pos[0] + 1, pos[1])
            elif dir == "L":
                pos = (pos[0] - 1, pos[1])
            else:
                raise Exception(f"Unknown direction {dir}")

            min_x, min_y = min(min_x, pos[0]), min(min_y, pos[1])
            max_x, max_y = max(max_x, pos[0]), max(max_y, pos[1])

            border.add(pos)

    return border, min_x, min_y, max_x, max_y

def find_inside(border, min_x, min_y, max_x, max_y, start_x, start_y):
    seen = set()
    inside = set()
    queue = [(start_x, start_y)]

    while queue:
        x, y = queue.pop(0)

        if (x, y) in seen:
            continue

        seen.add((x, y))

        if (x, y) in border:
            continue

        inside.add((x, y))

        if x < min_x or x > max_x or y < min_y or y > max_y:
            continue

        queue.append((x - 1, y))
        queue.append((x + 1, y))
        queue.append((x, y - 1))
        queue.append((x, y + 1))

    return inside

def show(border, inside, min_x, min_y, max_x, max_y):
    for y in range(min_y, max_y + 1):
        for x in range(min_x, max_x + 1):
            if (x, y) in inside:
                assert (x, y) not in border, f"({x}, {y}) is in both inside and border"
                print("·", end="")
            elif (x, y) in border:
                if (x, y) == (0, 0):
                    print("S", end="")
                else:
                    print("#", end="")
            else:
                print(" ", end="")
        print()

def print_and_assert(part, expected, actual):
    print(f"Part {part}: {actual}{' (sample)' if is_sample else ''}")
    assert actual == expected, f"Part {part} was {actual}, expected {expected}"

if __name__ == '__main__':
    dig_plan = parse()
    border, min_x, min_y, max_x, max_y = dig(dig_plan)

    # By looking at the output, we can see that for the sample, below and to the
    # right of the start position is a valid inside starting position for flood
    # filling. For my real input, one step to the right is fine.
    # Alternatively we could just try the flood fill and abort when min/max are
    # reached while filling, and then try with a different start position, but
    # this seems excessive.
    # There must be some clever way to determine the start position…
    start_x, start_y = (1, 1) if is_sample else (1, 0)

    inside = find_inside(border, min_x, min_y, max_x, max_y, start_x, start_y)
    # show(border, inside, min_x, min_y, max_x, max_y)

    part1 = len(border) + len(inside)

    print_and_assert(1, 62 if is_sample else 92758, part1)
    # print_and_assert(2, 21756 if is_sample else 4978, part2(lines))
