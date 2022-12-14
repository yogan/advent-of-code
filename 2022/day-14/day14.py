import sys, math

file = sys.argv[1] if len(sys.argv) > 1 else "day14.in"
is_sample = file != "day14.in"
sand_start = (500, 0)

def read_rocks():
    lines = open(file).read().splitlines()
    paths = [list(map(lambda x: tuple(map(int, x)),
                list(map(lambda x: x.split(","), line.split(" -> ")))))
            for line in lines]

    def update_min_max(x, y, dimensions):
        min_x, min_y, max_x, max_y = dimensions
        if (x < min_x):
            min_x = x
        elif (x > max_x):
            max_x = x
        if (y < min_y):
            min_y = y
        elif (y > max_y):
            max_y = y
        return (min_x, min_y, max_x, max_y)

    dimensions = sand_start + sand_start
    rocks = set()

    for path in paths:
        start = path[0]
        rocks.add(start)
        x1, y1 = start
        dimensions = update_min_max(x1, y1, dimensions)
        for coord in path[1:]:
            x2, y2 = coord
            dimensions = update_min_max(x2, y2, dimensions)
            if (x1 == x2):
                for y in range(min(y1, y2), max(y1, y2) + 1):
                    rocks.add((x1, y))
            else:
                for x in range(min(x1, x2), max(x1, x2) + 1):
                    rocks.add((x, y1))
            x1, y1 = x2, y2
    return rocks, dimensions

def print_field(sand, rocks, dimensions):
    min_x, min_y, max_x, max_y = dimensions
    for y in range(min_y, max_y + 1 + 2): # +2 for part2
        for x in range(min_x - 7, max_x + 1 + 10): # left/right for part2
            if (x, y) == sand_start:
                print("+", end="")
            elif (x, y) in sand:
                print("o", end="")
            elif (x, y) in rocks:
                print("#", end="")
            else:
                print(".", end="")
        print()
    print()

def simulate_sand(part1):
    def is_free(x, y, rocks, floor = None):
        return not ((x, y) in rocks) and (floor == None or y != floor)

    sand = set()
    sand_x, sand_y = sand_start
    rocks, dimensions = read_rocks()
    _, _, _, max_y = dimensions
    floor = None if part1 else max_y + 2

    while True:
        if part1 and sand_y >= max_y:
            break

        if is_free(sand_x, sand_y + 1, rocks, floor):
            sand_y += 1
            continue
        if is_free(sand_x - 1, sand_y + 1, rocks, floor):
            sand_x -= 1
            sand_y += 1
            continue
        if is_free(sand_x + 1, sand_y + 1, rocks, floor):
            sand_x += 1
            sand_y += 1
            continue

        sand.add((sand_x, sand_y))
        rocks.add((sand_x, sand_y))

        if (not part1) and (sand_x, sand_y) == sand_start:
            break

        sand_x, sand_y = sand_start
        # print_field(sand, rocks, dimensions) # each frame

    # print_field(sand, rocks, dimensions) # final state
    return len(sand)

def run():
    part1 = simulate_sand(True)
    assert(part1 == 24 if is_sample else 683)
    print("Part 1:", part1, "(sample)" if is_sample else "")

    part2 = simulate_sand(False)
    assert(part2 == 93 if is_sample else 28821)
    print("Part 2:", part2, "(sample)" if is_sample else "")

run()
