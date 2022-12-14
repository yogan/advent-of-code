import sys, math

file = sys.argv[1] if len(sys.argv) > 1 else "day14.in"
lines = open(file).read().splitlines()
paths = [list(map(lambda x: tuple(map(int, x)),
              list(map(lambda x: x.split(","), line.split(" -> ")))))
         for line in lines]

rocks = set()
sand = set()

min_x =  math.inf
max_x = -math.inf
min_y =  math.inf
max_y = -math.inf  # if sand_y >= max_y, it keeps on falling

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

def print_field():
    for y in range(min_y, max_y + 1):
        for x in range(min_x, max_x + 1):
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
    sand.add((sand_x, sand_y))
    rocks.add((sand_x, sand_y))
    sand_x, sand_y = sand_start
    # print_field()

part1 = len(sand)
assert(part1 == 683)
print("Part 1:", part1)
