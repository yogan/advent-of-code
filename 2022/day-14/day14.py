import sys, math

file = sys.argv[1] if len(sys.argv) > 1 else "day14.in"
is_sample = file != "day14.in"
lines = open(file).read().splitlines()
paths = [list(map(lambda x: tuple(map(int, x)),
              list(map(lambda x: x.split(","), line.split(" -> ")))))
         for line in lines]

sand_part_1 = set()
sand_part_2 = set()

def read_rocks():
    min_x =  math.inf
    max_x = -math.inf
    min_y =  math.inf
    max_y = -math.inf
    rocks = set()
    for path in paths:
        start = path[0]
        rocks.add(start)
        x1, y1 = start
        if (x1 < min_x):
            min_x = x1
        elif (x1 > max_x):
            max_x = x1
        if (y1 < min_y):
            min_y = y1
        elif (y1 > max_y):
            max_y = y1
        for coord in path[1:]:
            x2, y2 = coord
            if (x2 < min_x):
                min_x = x2
            elif (x2 > max_x):
                max_x = x2
            if (y2 < min_y):
                min_y = y2
            elif (y2 > max_y):
                max_y = y2
            if (x1 == x2):
                for y in range(min(y1, y2), max(y1, y2) + 1):
                    rocks.add((x1, y))
            else:
                for x in range(min(x1, x2), max(x1, x2) + 1):
                    rocks.add((x, y1))
            x1, y1 = x2, y2
    return rocks, min_x, max_x, min_y, max_y

def print_field(sand):
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

sand_start = (500, 0)
sand_x, sand_y = sand_start

# Part 1
rocks, min_x, max_x, min_y, max_y = read_rocks()

if (sand_x < min_x):
    min_x = sand_x
elif (sand_x > max_x):
    max_x = sand_x
if (sand_y < min_y):
    min_y = sand_y
elif (sand_y > max_y):
    max_y = sand_y

while True:
    if sand_y >= max_y:
        break
    if (sand_x, sand_y + 1) not in rocks:
        sand_y += 1
        continue
    if (sand_x - 1, sand_y + 1) not in rocks:
        sand_x -= 1
        sand_y += 1
        continue
    if (sand_x + 1, sand_y + 1) not in rocks:
        sand_x += 1
        sand_y += 1
        continue
    sand_part_1.add((sand_x, sand_y))
    rocks.add((sand_x, sand_y))
    sand_x, sand_y = sand_start
    # print_field(sand_part_1)

# Part 2
rocks, min_x, max_x, min_y, max_y = read_rocks()
for x in range(min_x - 300, max_x + 300):
    rocks.add((x, max_y + 2))
sand_x, sand_y = sand_start
while True:
    if (sand_x, sand_y + 1) not in rocks:
        sand_y += 1
        continue
    if (sand_x - 1, sand_y + 1) not in rocks:
        sand_x -= 1
        sand_y += 1
        continue
    if (sand_x + 1, sand_y + 1) not in rocks:
        sand_x += 1
        sand_y += 1
        continue
    sand_part_2.add((sand_x, sand_y))
    rocks.add((sand_x, sand_y))
    if (sand_x, sand_y) == sand_start:
        break
    sand_x, sand_y = sand_start
    # print_field(sand_part_2)

part1 = len(sand_part_1)
if (is_sample):
    assert(part1 == 24)
else:
    assert(part1 == 683)
print("Part 1:", part1)

part2 = len(sand_part_2)
if (is_sample):
    assert(part2 == 93)
else:
    assert(part2 == 28821)
print("Part 2:", part2)
