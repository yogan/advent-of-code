import sys, math

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
is_sample = filename == "sample.txt"

def parse():
    return [(dir, int(steps), color[2:-1]) for dir, steps, color in
            [line.split() for line in open(filename).readlines()]]

def convert_hex_color(color):
    dir = color[-1].translate(str.maketrans("0123", "RDLU"))
    steps = int(color[:-1], 16)
    return (dir, steps)

def trace_border(movements):
    pos = (0, 0)
    vertices = []
    border_len = 0

    for dir, steps in movements:
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
        border_len += steps

    assert vertices[-1] == (0, 0), f"Last vertex is {vertices[-1]}"

    return border_len, vertices

def shoelace_area(vertices):
    # https://en.wikipedia.org/wiki/Shoelace_formula
    # https://www.youtube.com/watch?v=0KjG8Pg6LGk
    return abs(sum(x0 * y1 - x1 * y0 for ((x0, y0), (x1, y1)) in
                   zip(vertices, vertices[1:] + [vertices[0]]))) // 2

def picks_theorem(area, boundary_points):
    # https://en.wikipedia.org/wiki/Pick%27s_theorem
    #
    # Pick's theorem: A = i + b/2 - 1
    #   A: area of the polygon
    #   i: number of points with inside the polygon
    #   b: number of points with on the boundary of the polygon
    #
    # We know A and b, so we can solve for i:
    return area - boundary_points // 2 + 1

def calculate_lava_volume(movements):
    boundary_points, vertices = trace_border(movements)
    area = shoelace_area(vertices)
    internal_points = picks_theorem(area, boundary_points)

    return internal_points + boundary_points

def print_and_assert(part, actual, expected):
    print(f"Part {part}: {actual}{' (sample)' if is_sample else ''}")
    assert actual == expected, f"Part {part} was {actual}, expected {expected}"

if __name__ == '__main__':
    dig_plan = parse()

    movements_part_1 = [(dir, steps) for dir, steps, _ in dig_plan]
    movements_part_2 = [convert_hex_color(color) for _, _, color in dig_plan]

    part1 = calculate_lava_volume(movements_part_1)
    part2 = calculate_lava_volume(movements_part_2)

    print_and_assert(1, part1, 62 if is_sample else 92758)
    print_and_assert(2, part2, 952408144115 if is_sample else 62762509300678)
