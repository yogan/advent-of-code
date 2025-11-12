import sys
import unittest

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename = sys.argv[1]
is_sample = filename!= "input.txt"

DEBUG = False
width = 7
empty_row = list("." * width)
pieces = [
    [
        list("..@@@@.")
    ],
    [
        list("...@..."),
        list("..@@@.."),
        list("...@..."),
    ],
    [
        list("..@@@.."), # intentionally flipped,
        list("....@.."), # as we add piece lines from
        list("....@.."), # bottom (↑) to top (↓)
    ],
    [
        list("..@...."),
        list("..@...."),
        list("..@...."),
        list("..@...."),
    ],
    [
        list("..@@..."),
        list("..@@..."),
    ]
]
states = {}

def parse(file = filename):
    with open(file) as f:
        return list(f.readlines()[0].strip())

def debug(*args):
    if DEBUG:
        print(*args)

def debug_field(field):
    if DEBUG:
        print_field(field)

def print_field(field):
    for i, line in enumerate(reversed(field)):
        i = len(field) - i - 1
        print(f"   {i:04d}  |{''.join(line)}|")
    print(f"    +{'-' * width}+\n")

def play_rock_tetris(movements, rocks_max, return_field = False):
    rocks = 0
    piece_idx = 0
    piece_btm_i = 0
    move_idx = 0
    field = []
    height = 0
    rows_from_cycle = 0

    while rocks < rocks_max:
        debug("\n-- LOOP -- Settled rocks:", rocks, "of", rocks_max, "--\n")

        # place new piece at the top (appends are bottom to top)
        piece     = pieces[piece_idx]
        piece_idx = (piece_idx + 1) % len(pieces)
        field.append(list(empty_row))
        field.append(list(empty_row))
        field.append(list(empty_row))
        for piece_line in piece:
            field.append(list(piece_line))
        height += 3 + len(piece)
        piece_btm_i = height - len(piece)
        debug(f"piece {piece_idx} placed, bottom = {piece_btm_i}:")
        debug_field(field)

        settled = False

        while not settled:
            # apply jet stream movement
            move     = movements[move_idx]
            move_idx = (move_idx + 1) % len(movements)
            side_free = True
            if (move == ">"):
                for i in range(piece_btm_i, piece_btm_i + len(piece)):
                    i = i - rows_from_cycle
                    # check for right wall
                    if field[i][-1] == "@":
                        side_free = False
                        debug("right side of piece hits wall, can't move right\n")
                        break
                    # check blocks right of piece blocks as well
                    for j in range(width - 1):
                        if field[i][j] == "@" and field[i][j + 1] == "#":
                            side_free = False
                            debug("right side of piece hits rock, can't move right\n")
                            break
                    else:
                        continue
                if side_free:
                    for i in range(piece_btm_i, piece_btm_i + len(piece)):
                        i = i - rows_from_cycle
                        # move @ blocks of piece to the right, going right to left
                        for j in range(width - 1, -1, -1):
                            if field[i][j] == "@":
                                field[i][j] = "."
                                field[i][j + 1] = "@"
            elif (move == "<"):
                for i in range(piece_btm_i, piece_btm_i + len(piece)):
                    i = i - rows_from_cycle
                    # check for left wall
                    if field[i][0] == "@":
                        side_free = False
                        debug("left side of piece hits wall, can't move left\n")
                        break
                    # check blocks left of piece blocks as well
                    for j in range(1, width):
                        if field[i][j] == "@" and field[i][j - 1] == "#":
                            side_free = False
                            debug("left side of piece hits rock, can't move left\n")
                            break
                    else:
                        continue
                if side_free:
                    for i in range(piece_btm_i, piece_btm_i + len(piece)):
                        i = i - rows_from_cycle
                        # move @ pieces of piece to the left, going left to right
                        for j in range(width):
                            if field[i][j] == "@":
                                field[i][j] = "."
                                field[i][j - 1] = "@"
                pass
            else:
                raise Exception(f"Unknown movement: {move}")
            if side_free:
                debug(f"movement \"{move}\" ({move_idx}) applied:")
                debug_field(field)

            # collision could be either hitting the bottom or another rock
            collision = False
            # if len of piece is len of field, we're at the bottom
            if len(piece) == height:
                collision = True
            else:
                # check for collision at indices below the falling piece
                for i in range(piece_btm_i, piece_btm_i + len(piece)):
                    i = i - rows_from_cycle
                    for j in range(width):
                        if field[i][j] == "@" and field[i - 1][j] == "#":
                            collision = True
                            break
                    else:
                        continue

            if collision:
                # rock settles, convert @ to #
                for i in range(piece_btm_i, piece_btm_i + len(piece)):
                    i = i - rows_from_cycle
                    for j in range(width):
                        if field[i][j] == "@":
                            field[i][j] = "#"

                rocks += 1
                settled = True

                if rows_from_cycle == 0:
                    columns = ":".join([str(d) for d in get_column_depths(field)])
                    key = (piece_idx, move_idx, columns)
                    if key in states:
                        prev_rocks, prev_height = states[key]
                        cycle_rocks = rocks - prev_rocks
                        cycle_height = height - prev_height
                        if cycle_rocks > 0:
                            debug("found a cycle:", key, "rocks:", cycle_rocks,
                                "height:", cycle_height)
                            debug("current height:", height, "rocks:", rocks)
                            cycles_to_add = (rocks_max - rocks) // cycle_rocks
                            debug("cycles to add:", cycles_to_add)
                            rows_from_cycle = cycles_to_add * cycle_height
                            height += rows_from_cycle
                            rocks += cycles_to_add * cycle_rocks
                            debug("new height:", height, "rocks:", rocks)
                    else:
                        states[key] = (rocks, height)

                debug("collision detected, rock settles:")
                debug_field(field)
            else:
                # fall down
                for i in range(piece_btm_i, piece_btm_i + len(piece)):
                    i = i - rows_from_cycle
                    for j in range(width):
                        if field[i][j] == "@":
                            field[i][j] = "."
                            field[i - 1][j] = "@"
                piece_btm_i -= 1
                # remove empty top row
                if field[-1] == empty_row:
                    field.pop(-1)
                    height -= 1
                debug("no collision, rock falls down:")
                debug_field(field)

    debug("==== DONE ==== Settled rocks:", rocks, "of", rocks_max, "====\n")
    return field if return_field else height

def get_column_depths(field):
    depths = [0] * width
    for col in range(width):
        for row in range(len(field) - 1, -1, -1):
            if field[row][col] == "#":
                break
            depths[col] += 1
    return depths

def part1():
    return play_rock_tetris(parse(), 2022)

def part2():
    return play_rock_tetris(parse(), 1000000000000)

class TestDay17(unittest.TestCase):
    def test_parse(self):
        movements = parse()
        if is_sample:
            self.assertEqual(40, len(movements))
            self.assertEqual(['>', '>', '>', '<'], movements[:4])
            self.assertEqual(['<', '<', '>', '>'], movements[-4:])
        else:
            self.assertEqual(10091, len(movements))
            self.assertEqual(['>', '>', '>', '<'], movements[:4])
            self.assertEqual(['>', '>', '<', '<'], movements[-4:])

    def test_play_rock_tetris(self):
        movements = parse("sample.txt")
        field = play_rock_tetris(movements, 1, True)
        expected = [list("..####.")]
        self.assertEqual(expected, field)

    def test_play_rock_tetris_two_rocks(self):
        movements = parse("sample.txt")
        field = play_rock_tetris(movements, 2, True)
        expected = list(reversed([
            list("...#..."),
            list("..###.."),
            list("...#..."),
            list("..####."),
        ]))
        self.assertEqual(expected, field)

    def test_play_rock_tetris_ten_rocks(self):
        movements = parse("sample.txt")
        field = play_rock_tetris(movements, 10, True)
        expected = list(reversed([
            list("....#.."),
            list("....#.."),
            list("....##."),
            list("##..##."),
            list("######."),
            list(".###..."),
            list("..#...."),
            list(".####.."),
            list("....##."),
            list("....##."),
            list("....#.."),
            list("..#.#.."),
            list("..#.#.."),
            list("#####.."),
            list("..###.."),
            list("...#..."),
            list("..####."),
        ]))
        self.assertEqual(expected, field)

    def test_get_column_depths(self):
        field = list(reversed([
            list("....#.."),
            list("....#.."),
            list("....##."),
            list("##..##."),
            list("######."),
            list(".###..."),
            list("..#...."),
            list(".####.."),
            list("....##."),
            list("....##."),
            list("....#.."),
            list("..#.#.."),
            list("..#.#.."),
            list("#####.."),
            list("..###.."),
            list("...#..."),
            list("..####."),
        ]))
        expected = [3, 3, 4, 4, 0, 2, 17]
        self.assertEqual(expected, get_column_depths(field))

    def test_part1(self):
       self.assertEqual(3068 if is_sample else 3175, part1())

    def test_part2(self):
        self.assertEqual(1514285714288 if is_sample else 1555113636385, part2())

if __name__ == '__main__':
    unittest.main(argv=sys.argv[:1], exit=False)
    print()

    res1 = part1()
    print(f"Part 1: {res1}", "(sample)" if is_sample else "")

    res2 = part2()
    print(f"Part 2: {res2}", "(sample)" if is_sample else "")
