import sys, math

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
is_sample = filename == "sample.txt"

def parse():
    return [(dir, int(steps), color[2:-1]) for dir, steps, color in
            [line.split() for line in open(filename).readlines()]]

def trace_border(plan):
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
                assert False, f"Unknown direction {dir}"

            border.add(pos)

    return border

def get_vertices(plan):
    pos = (0, 0)
    vertices = [pos]

    for dir, steps, _ in plan:
        if dir == "U":
            pos = (pos[0], pos[1] - steps)
        elif dir == "D":
            pos = (pos[0], pos[1] + steps)
        elif dir == "R":
            pos = (pos[0] + steps, pos[1])
        elif dir == "L":
            pos = (pos[0] - steps, pos[1])
        else:
            assert False, f"Unknown direction {dir}"

        vertices.append(pos)

    assert vertices[0] == vertices[-1], "Path does not end where it started"

    return vertices

# https://en.wikipedia.org/wiki/Shoelace_formula
# https://www.youtube.com/watch?v=0KjG8Pg6LGk
def shoelace_area(vertices):
    return abs(sum(x0 * y1 - x1 * y0 for ((x0, y0), (x1, y1)) in
                   zip(vertices, vertices[1:] + [vertices[0]]))) // 2

# https://en.wikipedia.org/wiki/Pick%27s_theorem
def picks_theorem(interior_points, boundary_points):
    return interior_points + boundary_points // 2 - 1

def print_and_assert(part, expected, actual):
    print(f"Part {part}: {actual}{' (sample)' if is_sample else ''}")
    assert actual == expected, f"Part {part} was {actual}, expected {expected}"

if __name__ == '__main__':
    dig_plan = parse()

    border   = trace_border(dig_plan)
    vertices = get_vertices(dig_plan)
    area     = shoelace_area(vertices)
    part1    = picks_theorem(area, len(border)) + 2 # no idea why + 2…

    print_and_assert(1, 62 if is_sample else 92758, part1)
    # print_and_assert(2, 21756 if is_sample else 4978, part2(lines))
