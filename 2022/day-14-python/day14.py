import os, sys, math
from tqdm import tqdm

# To generate animated gifs, requires Pillow, see
# https://pillow.readthedocs.io/en/stable/installation.html
from PIL import Image, ImageDraw
import imageio

file = sys.argv[1] if len(sys.argv) > 1 else "day14.in"
is_sample = file != "day14.in"
create_gifs = len(sys.argv) > 2 and sys.argv[2] == "--gifs"

sand_start  = (500, 0)

color_bg    = (150, 150, 150)
color_start = (179, 159,  97)
color_sand  = (194, 178, 128)
color_rock  = ( 42,  42 , 42)

frame_count = 0
skip_frames = 0

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

def simulate_sand(part1, create_gifs):
    def is_free(x, y, sand, rocks, floor = None):
        return not ((x, y) in sand or (x, y) in rocks) \
               and (floor == None or y != floor)

    sand = set()
    x, y = sand_start
    rocks, dimensions = read_rocks()
    _, _, _, max_y = dimensions
    floor = None if part1 else max_y + 2

    if create_gifs:
        global frame_count
        frame_count = 0
        if is_sample:
            total_frames = 170 if part1 else 699
        else:
            total_frames = 236 if part1 else 618
        pbar = tqdm(total=total_frames, desc="Generating frames")

    while True:
        if part1 and y >= max_y:
            break

        if is_free(x, y + 1, sand, rocks, floor):
            y += 1
            dimensions = update_min_max(x, y, dimensions)
            if create_gifs and is_sample:
                pbar.update()
                write_frame(part1, rocks, sand, (x, y))
            continue
        if is_free(x - 1, y + 1, sand, rocks, floor):
            x -= 1
            y += 1
            dimensions = update_min_max(x, y, dimensions)
            if create_gifs and is_sample:
                pbar.update()
                write_frame(part1, rocks, sand, (x, y))
            continue
        if is_free(x + 1, y + 1, sand, rocks, floor):
            x += 1
            y += 1
            dimensions = update_min_max(x, y, dimensions)
            if create_gifs and is_sample:
                pbar.update()
                write_frame(part1, rocks, sand, (x, y))
            continue

        sand.add((x, y))

        if create_gifs:
            if is_sample or frame_count < 50 or skip_frames == 0:
                pbar.update()
                write_frame(part1, rocks, sand)
                skip_frames = frame_count // 50
                if not part1:
                    skip_frames *= 8
            else:
                skip_frames -= 1

        if (not part1) and (x, y) == sand_start:
            break

        x, y = sand_start
        # print_field(sand, rocks, dimensions) # each frame

    # print_field(sand, rocks, dimensions) # final state
    return sand, rocks, dimensions

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

def write_frame(is_part1, rocks, sand, additional_sand_point = None):
    if is_part1:
        width  = 17 if is_sample else 75
        height = 10 if is_sample else 176
    else:
        width  = 23 if is_sample else 375
        height = 12 if is_sample else 176

    resize_factor = 24 if is_sample else 8

    if (additional_sand_point is not None):
        sand = list(sand) # copy to avoid modifying original
        sand.append(additional_sand_point)

    def shift_pixels(pixels, width):
        offset_x = sand_start[0] - width // 2
        if not is_sample and is_part1:
            offset_x += 25
        return [(x - offset_x, y) for x, y in pixels]

    image = Image.new('RGB', (width, height), color_bg)
    draw  = ImageDraw.Draw(image)

    if not is_part1:
        floor_pixels = [(x, height - 1) for x in range(width)]
        draw.point(floor_pixels, fill=color_rock)

    rock_pixels      = shift_pixels(rocks,        width)
    sand_start_pixel = shift_pixels([sand_start], width)
    sand_pixels      = shift_pixels(sand,         width)

    draw.point(rock_pixels,      fill=color_rock)
    draw.point(sand_pixels,      fill=color_sand)
    draw.point(sand_start_pixel, fill=color_start)

    (width, height) = (image.width * resize_factor, image.height * resize_factor)
    image = image.resize((width, height), Image.Resampling.NEAREST)

    global frame_count
    frame_count += 1
    dir_name = get_dir_name(is_part1)
    os.makedirs(dir_name, exist_ok=True)
    image.save(f"{dir_name}/frame_{frame_count:05d}.gif")

base_dir = "/tmp/aoc2022day14/"

def get_dir_name(is_part1):
    dir_name = base_dir + "part1/"  if is_part1  else base_dir + "part2/"
    dir_name = dir_name + "sample/" if is_sample else dir_name + "input/"
    return dir_name

def get_gif_path(is_part1, ):
    path = base_dir + "part1.gif" if is_part1 else base_dir + "part2.gif"
    if is_sample:
        path = path.replace(".gif", "_sample.gif")
    return path

def create_gif(is_part1):
    dir_name   = get_dir_name(is_part1)
    gif_path   = get_gif_path(is_part1)
    images     = []
    durations  = []
    if is_sample:
        frame_rate = 25 if is_part1 else 40
    else:
        frame_rate = 100 # 100 fps = 10ms per frame is the limit for GIFs

    pbar = tqdm(total=frame_count, desc="Create GIF from frames")

    for i in range(1, frame_count + 1):
        pbar.update()
        filename = f"{dir_name}/frame_{str(i).zfill(5)}.gif"
        images.append(imageio.v2.imread(filename))
        if i == 1:
            durations.append(0.5) # first frame: 500ms
        elif i == frame_count:
            durations.append(3.0) # last frame: 3000ms
        else:
            durations.append(1.0 / frame_rate) # default frame rate

    imageio.mimsave(gif_path, images, fps=frame_rate, duration=durations,
                    subrectangles=True, palettesize=4)
    print(f"GIF created: {gif_path} ({frame_count} frames)\n")

def part(num, create_gifs):
    is_part1 = num == 1

    if create_gifs:
        if is_sample:
            print(f"Creating GIF for part {num} (sample)…")
        else:
            print(f"Creating GIF for part {num}…")
            if not is_part1:
                print("This will take a while (a few minutes). Be patient!")

    sand, rocks, dimensions = simulate_sand(is_part1, create_gifs)

    if create_gifs:
        create_gif(is_part1)
    else:
        sand_amount = len(sand)
        if (is_part1):
            assert(sand_amount == 24 if is_sample else 683)
        else:
            assert(sand_amount == 93 if is_sample else 28821)
        print(f"Part {num}: {sand_amount}", "(sample)" if is_sample else "")

part(1, create_gifs)
part(2, create_gifs)
