import unittest, sys

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename = sys.argv[1]
sys.argv = sys.argv[:1] # strip args, they scare the unittest module
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
    # reverse field without modifying it
    for i, line in enumerate(reversed(field)):
        i = len(field) - i - 1
        print(f" {i:02d} |{''.join(line)}|")
    print(f"    +{'-' * width}+\n")

def play_rock_tetris(movements, rocks_max):
    rocks = 0
    piece_idx = 0
    piece_btm_i = 0
    move_idx = 0
    field = []

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
        piece_btm_i = len(field) - len(piece)
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
                        # move @ blocks of piece to the right, going right to left
                        for j in range(width - 1, -1, -1):
                            if field[i][j] == "@":
                                field[i][j] = "."
                                field[i][j + 1] = "@"
            elif (move == "<"):
                for i in range(piece_btm_i, piece_btm_i + len(piece)):
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
            if len(piece) == len(field):
                collision = True
            else:
                # check for collision at indices below the falling piece
                for i in range(piece_btm_i, piece_btm_i + len(piece)):
                    for j in range(width):
                        if field[i][j] == "@" and field[i - 1][j] == "#":
                            collision = True
                            break
                    else:
                        continue

            if collision:
                # rock settles, convert @ to #
                for i in range(piece_btm_i, piece_btm_i + len(piece)):
                    for j in range(width):
                        if field[i][j] == "@":
                            field[i][j] = "#"
                rocks += 1
                settled = True
                debug("collision detected, rock settles:")
                debug_field(field)
            else:
                # fall down
                for i in range(piece_btm_i, piece_btm_i + len(piece)):
                    for j in range(width):
                        if field[i][j] == "@":
                            field[i][j] = "."
                            field[i - 1][j] = "@"
                piece_btm_i -= 1
                # remove empty top row
                if field[-1] == empty_row:
                    field.pop(-1)
                debug("no collision, rock falls down:")
                debug_field(field)

    debug("==== DONE ==== Settled rocks:", rocks, "of", rocks_max, "====\n")
    return field

def part1():
    movements = parse()
    field = play_rock_tetris(movements, 2022)
    return len(field)

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
        field = play_rock_tetris(movements, 1)
        expected = [list("..####.")]
        self.assertEqual(expected, field)

    def test_play_rock_tetris_two_rocks(self):
        movements = parse("sample.txt")
        field = play_rock_tetris(movements, 2)
        expected = list(reversed([
            list("...#..."),
            list("..###.."),
            list("...#..."),
            list("..####."),
        ]))
        self.assertEqual(expected, field)

    def test_play_rock_tetris_ten_rocks(self):
        movements = parse("sample.txt")
        field = play_rock_tetris(movements, 10)
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

    def test_part1(self):
        self.assertEqual(3068 if is_sample else 3175, part1())

if __name__ == '__main__':
    unittest.main(exit=False)
    print()

    res1 = part1()
    print(f"Part 1: {res1}", "(sample)" if is_sample else "")

    # res2 = part2()
    # print(f"Part 2: {res2}", "(sample)" if is_sample else "")
