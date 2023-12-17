import sys, time, curses
from collections import defaultdict
from copy import deepcopy

if len(sys.argv) == 3 and "--visualize" in sys.argv:
    sys.argv.remove("--visualize")
    visualize = True
else:
    visualize = False
if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
is_sample = filename == "sample.txt"

def parse():
    return [list(x.strip()) for x in open(filename).readlines()]

def get_directions(value, dir):
    if value == '.':
        return [dir]

    table = {
        '/':  {'R': ['U'],      'L': ['D'],      'U': ['R'],      'D': ['L']},
        '\\': {'R': ['D'],      'L': ['U'],      'U': ['L'],      'D': ['R']},
        '|':  {'R': ['U', 'D'], 'L': ['U', 'D'], 'U': ['U'],      'D': ['D']},
        '-':  {'R': ['R'],      'L': ['L'],      'U': ['R', 'L'], 'D': ['R', 'L']}
    }

    return table[value][dir]

def move(H, W, row, col, dir):
    if dir == 'R' and col < W - 1:
        return (row, col + 1, dir)
    elif dir == 'L' and col > 0:
        return (row, col - 1, dir)
    elif dir == 'U' and row > 0:
        return (row - 1, col, dir)
    elif dir == 'D' and row < H - 1:
        return (row + 1, col, dir)

    return None

# HINT: use xterm-colortest script to get color numbers

# DIR_COLORS = range(196, 220) # maaany colors (red/orange/yellow)
# DIR_COLORS = range(196, 202) # red to pink
# DIR_COLORS = [196, 197, 198, 160, 161, 162] # reds, very similar
# DIR_COLORS = [112, 113, 118, 119] # bright greens, very similar
DIR_COLORS = list(range(112, 117)) + list(range(123, 117, -1)) # green/blue gradient
DC = len(DIR_COLORS)

# MIRRORS_COLOR = 33 # deep blue
# MIRRORS_COLOR = 103 # blueish gray
MIRRORS_COLOR = 63
MC = 2 * DC + 1

def mirror_color():
    return curses.color_pair(MC)

def laser_color(steps, on_mirror):
    if on_mirror:
        return curses.color_pair(steps % DC + DC + 1)
    else:
        return curses.color_pair(steps % DC + 1)

def map_mirrors(char):
    return char.translate(char.maketrans("/\\|-.", "╱╲│─ "))

def map_dirs(char):
    return char.translate(char.maketrans("RLUD", "→←↑↓"))

def delay(steps):
    if steps < 5:
        factor = 0.3
    elif steps < 10:
        factor = 0.2
    elif steps < 20:
        factor = 0.1
    elif steps < 40:
        factor = 0.05
    elif steps < 100:
        factor = 0.025
    elif steps < 500:
        factor = 0.0125
    else:
        factor = 0.008
    time.sleep(factor)

def init_curses(layout):
    stdscr = curses.initscr()
    curses.noecho()
    curses.cbreak()
    curses.curs_set(False)
    stdscr.clear()
    stdscr.keypad(True)

    curses.start_color()
    curses.use_default_colors()
    no_bg = -1
    black_bg = 0
    blue_bg = 4
    for i in range(DC):
        curses.init_pair(1 + i,      DIR_COLORS[i], black_bg)
        curses.init_pair(1 + i + DC, DIR_COLORS[i], blue_bg)
    curses.init_pair(MC, MIRRORS_COLOR, black_bg)

    return stdscr

def start_visualization(stdscr, layout):
    cropped_layout = [row[:int(curses.COLS)] for row in layout[:curses.LINES]]
    pewpew(cropped_layout, (0, 0, 'R'), stdscr)

def cleanup_curses(stdscr):
    # wait for key press
    while True:
        c = stdscr.getch()
        break

    stdscr.keypad(False)
    curses.nocbreak()
    curses.curs_set(True)
    curses.echo()
    # curses.endwin() # this clears the screen

def visualize_mirrors(stdscr, H, W, layout):
    c = 0
    for row in range(H):
        for col in range(W):
            char = map_mirrors(layout[row][col])
            stdscr.addstr(row, col, char, mirror_color())
    stdscr.refresh()
    time.sleep(1)

def visualize_laser(stdscr, row, col, dir, steps, layout):
    on_mirror = layout[row][col] in "/\\-|"
    color = laser_color(steps, on_mirror)
    stdscr.addstr(row, col, map_dirs(dir), color)
    stdscr.refresh()
    delay(steps)

def pewpew(layout, start, stdscr=None):
    H, W = len(layout), len(layout[0])
    visited = set()
    visited.add(start)
    queue = [start]

    if stdscr:
        visualize_mirrors(stdscr, H, W, layout)
        steps = 0

    while queue:
        row, col, dir = queue.pop(0)

        if stdscr:
            visualize_laser(stdscr, row, col, dir, steps, layout)
            steps += 1

        next_dirs = get_directions(layout[row][col], dir)
        for next_dir in next_dirs:
            next = move(H, W, row, col, next_dir)
            if next and next not in visited:
                visited.add(next)
                queue.append(next)

    return len(set([(row, col) for row, col, _ in visited]))

def start_positions(layout):
    H, W = len(layout), len(layout[0])

    top_row =    [(    0, col,   'D') for col in range(W)]
    bottom_row = [(H - 1, col,   'U') for col in range(W)]
    left_col =   [(  row, 0,     'R') for row in range(H)]
    right_col =  [(  row, W - 1, 'L') for row in range(H)]

    # starting with left_col so that part 1 start is the first entry
    return [*left_col, *top_row, *bottom_row, *right_col]

def print_and_assert(part, expected, actual):
    print(f"Part {part}: {actual}{' (sample)' if is_sample else ''}")
    assert actual == expected, f"Part {part} was {actual}, expected {expected}"

if __name__ == '__main__':
    layout = parse()

    if visualize:
        stdscr = init_curses(layout)
        start_visualization(stdscr, layout)
        cleanup_curses()
        exit()

    starts = start_positions(layout)
    energy_levels = [pewpew(layout, start) for start in starts]
    part1 = energy_levels[0]
    part2 = max(energy_levels)

    print_and_assert(1, 46 if is_sample else 8551, part1)
    print_and_assert(2, 51 if is_sample else 8754, part2)
