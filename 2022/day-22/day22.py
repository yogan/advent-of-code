import unittest, sys, re

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they scare the unittest module

is_input  = filename == "input.txt"               # should pass part 1 and 2
is_sample = filename == "sample.txt"              # should pass only part 1
is_trans  = filename == "sample_transformed.txt"  # should pass only part 2

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

# This assumes that the cube is flattened in a certain way. Valid for input.txt
# and sample_transformed.txt (not sample.txt!)
#
#        c    d   2d  3d
#    r    0  34  78  11
#     0       11112222
#             11112222
#             11112222
#     3       11112222
#  d  4       3333
#             3333
#             3333
#     7       3333
# 2d  8   44445555
#         44445555
#         44445555
#    11   44445555
# 3d 12   6666
#         6666
#         6666
#    14   6666
def calc_face(r, c, d):
    if 0 <= r < d:
        if d <= c < 2*d:
            return 1
        elif 2*d <= c < 3*d:
            return 2
    elif d <= r < 2*d:
        if d <= c < 2*d:
            return 3
    elif 2*d <= r < 3*d:
        if 0 <= c < d:
            return 4
        elif d <= c < 2*d:
            return 5
    elif 3*d <= r < 4*d:
        if 0 <= c < d:
            return 6

    raise ValueError(f"{r, c} is off the cube for side length {d}")

def walk_cube(r, c, steps, direction, side_len, rows, min_max):
    # NOTES:
    # min_max probably useless, use side_len instead

    face = calc_face(r, c, side_len)
    # print(f"face {face} at {r},{c} facing {dir_to_str(direction)}")

    # TODO rewrite stuff below for cube, watch for changing faces

    # TODO
    d_next = direction

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

    return r, c, d_next

def travel(rows, min_max, path, cube=False):
    direction = Direction.RIGHT
    r, c = (0, min_max[0][0])
    if cube:
        side_len = 50 if is_input else 4

    assert rows[r][col_idx(c, r, min_max)] == ".", "can't start on a wall"
    # print(f"    START    {r},{c} direction: {dir_to_str(direction)}")

    for move in path:
        if move in ['L', 'R']:
            direction = turn(direction, move)
            # print(f" turn:  {move} -> {r},{c} direction: {dir_to_str(direction)}")
        else:
            assert move > 0, f"invalid steps {move}"
            if cube:
                r, c, direction = walk_cube(r, c, move, direction, side_len, rows, min_max)
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

    def test_calc_face_sample(self):
        side_len = 4  # side length of sample(_transformed).txt

        self.assertEqual(1, calc_face( 0,  4, side_len))
        self.assertEqual(1, calc_face( 0,  7, side_len))
        self.assertEqual(1, calc_face( 3,  4, side_len))
        self.assertEqual(1, calc_face( 3,  7, side_len))

        self.assertEqual(2, calc_face( 0,  8, side_len))
        self.assertEqual(2, calc_face( 0, 11, side_len))
        self.assertEqual(2, calc_face( 3,  8, side_len))
        self.assertEqual(2, calc_face( 3, 11, side_len))

        self.assertEqual(3, calc_face( 4,  4, side_len))
        self.assertEqual(3, calc_face( 4,  7, side_len))
        self.assertEqual(3, calc_face( 7,  4, side_len))
        self.assertEqual(3, calc_face( 7,  7, side_len))

        self.assertEqual(4, calc_face( 8,  0, side_len))
        self.assertEqual(4, calc_face( 8,  3, side_len))
        self.assertEqual(4, calc_face(11,  0, side_len))
        self.assertEqual(4, calc_face(11,  3, side_len))

        self.assertEqual(5, calc_face( 8,  4, side_len))
        self.assertEqual(5, calc_face( 8,  7, side_len))
        self.assertEqual(5, calc_face(11,  4, side_len))
        self.assertEqual(5, calc_face(11,  7, side_len))

        self.assertEqual(6, calc_face(12,  0, side_len))
        self.assertEqual(6, calc_face(12,  3, side_len))
        self.assertEqual(6, calc_face(15,  0, side_len))
        self.assertEqual(6, calc_face(15,  3, side_len))

        # off the cube
        self.assertRaises(ValueError, lambda: calc_face( 0,  0, side_len))
        self.assertRaises(ValueError, lambda: calc_face( 0, 12, side_len))
        self.assertRaises(ValueError, lambda: calc_face( 4,  8, side_len))
        self.assertRaises(ValueError, lambda: calc_face( 7,  3, side_len))
        self.assertRaises(ValueError, lambda: calc_face( 7,  8, side_len))
        self.assertRaises(ValueError, lambda: calc_face( 8,  8, side_len))
        self.assertRaises(ValueError, lambda: calc_face(11,  8, side_len))
        self.assertRaises(ValueError, lambda: calc_face(12,  4, side_len))
        self.assertRaises(ValueError, lambda: calc_face(15,  4, side_len))
        self.assertRaises(ValueError, lambda: calc_face(16,  0, side_len))
        self.assertRaises(ValueError, lambda: calc_face(16,  1, side_len))
        self.assertRaises(ValueError, lambda: calc_face(16, 15, side_len))

    def test_calc_face_input(self):
        side_len = 50  # side length of input.txt

        self.assertEqual(1, calc_face(  0,  50, side_len))
        self.assertEqual(1, calc_face(  0,  99, side_len))
        self.assertEqual(1, calc_face( 49,  50, side_len))
        self.assertEqual(1, calc_face( 49,  99, side_len))

        self.assertEqual(2, calc_face(  0, 100, side_len))
        self.assertEqual(2, calc_face(  0, 149, side_len))
        self.assertEqual(2, calc_face( 49, 100, side_len))
        self.assertEqual(2, calc_face( 49, 149, side_len))

        self.assertEqual(3, calc_face( 50,  50, side_len))
        self.assertEqual(3, calc_face( 50,  99, side_len))
        self.assertEqual(3, calc_face( 99,  50, side_len))
        self.assertEqual(3, calc_face( 99,  99, side_len))

        self.assertEqual(4, calc_face(100,   0, side_len))
        self.assertEqual(4, calc_face(100,  49, side_len))
        self.assertEqual(4, calc_face(149,   0, side_len))
        self.assertEqual(4, calc_face(149,  49, side_len))

        self.assertEqual(5, calc_face(100,  50, side_len))
        self.assertEqual(5, calc_face(100,  99, side_len))
        self.assertEqual(5, calc_face(149,  50, side_len))
        self.assertEqual(5, calc_face(149,  99, side_len))

        self.assertEqual(6, calc_face(150,   0, side_len))
        self.assertEqual(6, calc_face(150,  49, side_len))
        self.assertEqual(6, calc_face(199,   0, side_len))
        self.assertEqual(6, calc_face(199,  49, side_len))

        # off the cube
        self.assertRaises(ValueError, lambda: calc_face(  0,  0, side_len))
        self.assertRaises(ValueError, lambda: calc_face(  0, 49, side_len))
        self.assertRaises(ValueError, lambda: calc_face( 99, 49, side_len))
        self.assertRaises(ValueError, lambda: calc_face(150, 50, side_len))
        self.assertRaises(ValueError, lambda: calc_face(199, 50, side_len))
        self.assertRaises(ValueError, lambda: calc_face(200,  0, side_len))
        self.assertRaises(ValueError, lambda: calc_face(200, 33, side_len))
        self.assertRaises(ValueError, lambda: calc_face(200, 66, side_len))

if __name__ == '__main__':
    unittest.main(exit=False)
    print()

    res1 = part1()
    if is_sample:
        assert res1 == 6032
    elif is_input:
        assert res1 == 97356
    print(f"Part 1: {res1}", "(sample)" if is_sample or is_trans else "")
    print()

    if is_trans or is_input:
        res2 = part2()
        print(f"Part 2: {res2}", "(sample)" if is_sample or is_trans else "")
