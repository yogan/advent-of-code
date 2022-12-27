import unittest, sys, re

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they scare the unittest module
is_input  = filename == "input.txt"
is_sample = filename == "sample.txt"
is_extra  = filename == "sample_extra.txt"
is_trans  = filename == "sample_transformed.txt"

def parse(filename=filename):
    with open(filename) as f:
        lines = f.readlines()
        rows, min_max = [], []
        r = 0
        while True:
            line = lines[r]
            line = line.replace("\n", "")
            if len(line) == 0:
                break
            c_min = 0
            rows.append([])
            for c in range(len(line)):
                if line[c] == " ":
                    c_min += 1
                    continue
                rows[r] += line[c]
            min_max.append((c_min, c))
            r += 1

        path_tokens = re.split(r'([RL])', lines[r+1].strip())
        path = [int(t) if t.isdigit() else t for t in path_tokens]

        return (rows, min_max, path)

class Direction:
    RIGHT = 0
    DOWN  = 1
    LEFT  = 2
    UP    = 3

def dir_to_str(direction):
    match direction:
        case Direction.UP:
            return "↑"
        case Direction.RIGHT:
            return "→"
        case Direction.DOWN:
            return "↓"
        case Direction.LEFT:
            return "←"

    assert False, f"invalid direction {direction}"

def turn(direction, turn):
    match turn:
        case 'L':
            return (direction - 1) % 4
        case 'R':
            return (direction + 1) % 4

    assert False, f"invalid turn {turn}"

def col_idx(c, r, min_max):
    return c - min_max[r][0]

def walk(r, c, steps, direction, rows, min_max):
    assert steps > 0, f"invalid steps {steps}"

    match direction:
        case Direction.LEFT | Direction.RIGHT:
            dc = 1 if direction == Direction.RIGHT else -1
            for _ in range(steps):
                c_next = c + dc
                # print(f"             {r},{c}", list(min_max[r]), c_next)
                (c_min, c_max) = min_max[r]
                if c_next < c_min:
                    c_next = c_max  # left edge reached - wrap to right
                elif c_next > c_max:
                    c_next = c_min  # right edge reached - wrap to left
                target = rows[r][col_idx(c_next, r, min_max)]
                if target == '#':
                    break
                c = c_next

        case Direction.UP | Direction.DOWN:
            dr = 1 if direction == Direction.DOWN else -1
            for _ in range(steps):
                r_next = r + dr
                # print(f"             {r},{c}", list(min_max[r]), r_next)
                if r_next < 0:
                    r_next = len(rows) - 1  # top edge - wrap to bottom
                elif r_next >= len(rows):
                    r_next = 0              # bottom edge - wrap to top
                # we need to keep moving if r_next is out of bounds
                while True:
                    valid = min_max[r_next][0] <= c <= min_max[r_next][1]
                    if valid:
                        break
                    r_next += dr
                    if r_next < 0:
                        r_next = len(rows) - 1  # top edge - wrap to bottom
                    elif r_next >= len(rows):
                        r_next = 0              # bottom edge - wrap to top
                target = rows[r_next][col_idx(c, r_next, min_max)]
                if target == '#':
                    break
                r = r_next

    return r, c

def travel(rows, min_max, path, cube=False):
    direction = Direction.RIGHT
    r, c = (0, min_max[0][0])
    if cube:
        d = 50 if is_input else 4

    assert rows[r][col_idx(c, r, min_max)] == ".", "can't start on a wall"
    # print(f"    START    {r},{c} direction: {dir_to_str(direction)}")

    for move in path:
        if move in ['L', 'R']:
            direction = turn(direction, move)
            # print(f" turn:  {move} -> {r},{c} direction: {dir_to_str(direction)}")
        else:
            r, c = walk(r, c, move, direction, rows, min_max)
            # print(f"steps: {move:2d} -> {r},{c} {dir_to_str(direction)}")

    return (r+1, c+1, direction)

def password(r, c, direction):
    return 1000 * r + 4 * c + direction

def part1():
    rows, min_max, path = parse()
    (r, c, direction) = travel(rows, min_max, path)
    print(f"Final position: {r+1}, {c+1} {dir_to_str(direction)}")
    return password(r, c, direction)

def part2():
    rows, min_max, path = parse()
    (r, c, direction) = travel(rows, min_max, path, cube=True)
    print(f"Final position: {r+1}, {c+1} {dir_to_str(direction)}")
    return password(r, c, direction)

class TestDay22(unittest.TestCase):
    def test_parse(self):
        rows, min_max, path = parse()
        first_line_min_max       = (min_max[0][0], min_max[0][1])
        last_line_min_max        = (min_max[-1][0], min_max[-1][1])
        first_row                = "".join(rows[0])
        last_row                 = "".join(rows[-1])

        if is_sample:
            self.assertEqual(12, len(rows))
            self.assertEqual((8, 11), first_line_min_max)
            self.assertEqual((8, 15), last_line_min_max)
            self.assertEqual("...#", first_row)
            self.assertEqual("......#.", last_row)
            self.assertEqual([10,'R',5,'L',5,'R',10,'L',4,'R',5,'L',5], path)
        elif is_input:
            self.assertEqual(200, len(rows))
            self.assertEqual((50, 149), first_line_min_max)
            self.assertEqual(( 0,  49), last_line_min_max)

    def test_turn(self):
        self.assertEqual(Direction.LEFT,  turn(Direction.UP,    'L')) # ↑ L = ←
        self.assertEqual(Direction.RIGHT, turn(Direction.UP,    'R')) # ↑ R = →

        self.assertEqual(Direction.UP,    turn(Direction.RIGHT, 'L')) # → L = ↑
        self.assertEqual(Direction.DOWN,  turn(Direction.RIGHT, 'R')) # → R = ↓

        self.assertEqual(Direction.RIGHT, turn(Direction.DOWN,  'L')) # ↓ L = →
        self.assertEqual(Direction.LEFT,  turn(Direction.DOWN,  'R')) # ↓ R = ←

        self.assertEqual(Direction.DOWN,  turn(Direction.LEFT,  'L')) # ← L = ↓
        self.assertEqual(Direction.UP,    turn(Direction.LEFT,  'R')) # ← R = ↑

    def test_travel(self):
        rows, min_max, path = parse()
        (r, c, direction) = travel(rows, min_max, path)
        if is_sample:
            self.assertEqual((6, 8, Direction.RIGHT), (r, c, direction))
        elif is_input:
            self.assertEqual((97, 89, Direction.RIGHT), (r, c, direction))

if __name__ == '__main__':
    if is_input or is_sample:
        unittest.main(exit=False)
        print()

    res1 = part1()
    if is_sample or is_trans:
        assert res1 == 6032
    elif is_input:
        assert res1 == 97356
    print(f"Part 1: {res1}", "(sample)" if is_sample else "")
    print()

    res2 = part2()
    print(f"Part 2: {res2}", "(sample)" if is_sample else "")
