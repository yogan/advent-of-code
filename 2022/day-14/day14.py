import sys, math

# To generate animated gifs, requires Pillow, see
# https://pillow.readthedocs.io/en/stable/installation.html
from PIL import Image, ImageDraw

file = sys.argv[1] if len(sys.argv) > 1 else "day14.in"
is_sample = file != "day14.in"
sand_start = (500, 0)

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

def read_rocks():
    lines = open(file).read().splitlines()
    paths = [list(map(lambda x: tuple(map(int, x)),
                list(map(lambda x: x.split(","), line.split(" -> ")))))
            for line in lines]

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

    return list(rocks), dimensions

def simulate_sand(part1):
    def is_free(x, y, sand, rocks, floor = None):
        return not ((x, y) in sand or (x, y) in rocks) \
               and (floor == None or y != floor)

    def store_frame(sand_frames, sand, additional_point = None):
        if (is_sample):
            sand_copy = list(sand)
            if (additional_point is not None):
                sand_copy.append(additional_point)
            sand_frames.append(sand_copy)

    sand = set()
    sand_frames = []
    x, y = sand_start
    rocks, dimensions = read_rocks()
    _, _, _, max_y = dimensions
    floor = None if part1 else max_y + 2

    while True:
        if part1 and y >= max_y:
            break

        if is_free(x, y + 1, sand, rocks, floor):
            y += 1
            store_frame(sand_frames, sand, (x, y))
            dimensions = update_min_max(x, y, dimensions)
            continue
        if is_free(x - 1, y + 1, sand, rocks, floor):
            x -= 1
            y += 1
            store_frame(sand_frames, sand, (x, y))
            dimensions = update_min_max(x, y, dimensions)
            continue
        if is_free(x + 1, y + 1, sand, rocks, floor):
            x += 1
            y += 1
            store_frame(sand_frames, sand, (x, y))
            dimensions = update_min_max(x, y, dimensions)
            continue

        sand.add((x, y))
        store_frame(sand_frames, sand)

        if (not part1) and (x, y) == sand_start:
            break

        x, y = sand_start
        # print_field(sand, rocks, dimensions) # each frame

    # print_field(sand, rocks, dimensions) # final state
    return sand, rocks, sand_frames, dimensions

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

def create_gif(sand_frames, rocks, dimensions, is_part1):
    def shift_pixels(pixels, min_x, min_y):
        return [(x - min_x, y - min_y) for x, y in pixels]

    min_x, min_y, max_x, max_y = dimensions
    assert(min_x >= 0 and min_y >= 0)
    if (not is_part1):
        max_y += 1
    dimensions = (max_x - min_x + 1, max_y - min_y + 1)
    resize_factor = 30

    images      = []
    color_bg    = (150, 150, 150)
    color_start = (179, 159,  97)
    color_sand  = (194, 178, 128)
    color_rock  = ( 42,  42 , 42)

    if not is_part1:
        for x in range(min_x, max_x + 1):
            rocks.append((x, max_y))
    rock_pixels = shift_pixels(rocks, min_x, min_y)

    for i, sand in enumerate(sand_frames):
        # print("Frame", i, "/", len(sand_frames))
        im = Image.new('RGB', dimensions, color_bg)
        draw = ImageDraw.Draw(im)

        draw.point(shift_pixels([sand_start], min_x, min_y), fill=color_start)
        draw.point(shift_pixels(sand, min_x, min_y), fill=color_sand)
        draw.point(rock_pixels, fill=color_rock)

        (width, height) = (im.width * resize_factor, im.height * resize_factor)
        im_resized = im.resize((width, height), Image.Resampling.NEAREST)
        images.append(im_resized)

    filename = ("part1.gif" if is_part1 else "part2.gif")
    filename = filename.replace(".gif", "_sample.gif")
    images[0].save(filename,
        save_all=True, append_images=images[1:],
        optimize=False, duration=40, loop=0)
    print(f"Wrote {filename} ({len(images)} frames)")

def part(num):
    is_part1 = num == 1

    sand, rocks, sand_frames, dimensions = simulate_sand(is_part1)
    sand_amount = len(sand)

    if (is_part1):
        assert(sand_amount == 24 if is_sample else 683)
    else:
        assert(sand_amount == 93 if is_sample else 28821)

    print(f"Part {num}: {sand_amount}", "(sample)" if is_sample else "")

    if (is_sample):
        create_gif(sand_frames, rocks, dimensions, is_part1)
    else:
        print("Skipping gif creation (only works for small sample currently)")

part(1)
part(2)
