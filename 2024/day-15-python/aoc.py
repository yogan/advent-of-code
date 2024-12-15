import sys
import unittest


def parse(filename):
    map, moves = open(filename).read().strip().split("\n\n")
    robot = None
    boxes = []
    walls = []

    for r, row in enumerate(map.split("\n")):
        for c, cell in enumerate(row):
            if cell == "@":
                robot = (r, c)
            elif cell == "O":
                boxes.append((r, c))
            elif cell == "#":
                walls.append((r, c))

    return robot, boxes, walls, moves.replace("\n", "")


def show(robot, boxes, walls):
    max_r, max_c = walls[-1]
    for r in range(max_r + 1):
        for c in range(max_c + 1):
            if (r, c) == robot:
                print("@", end="")
            elif (r, c) in boxes:
                print("O", end="")
            elif (r, c) in walls:
                print("#", end="")
            else:
                print(".", end="")
        print()


def simulate(robot, boxes, walls, moves):
    for move in moves:
        if move == "^":
            dr, dc = -1, 0
        elif move == "v":
            dr, dc = 1, 0
        elif move == "<":
            dr, dc = 0, -1
        elif move == ">":
            dr, dc = 0, 1
        else:
            raise ValueError(f"Unknown move: {move}")

        r, c = robot
        new_robot = (r + dr, c + dc)

        if new_robot in walls:
            continue

        can_move = True

        if new_robot in boxes:
            boxes_to_move = []
            box_r, box_c = new_robot
            while (box_r, box_c) in boxes:
                boxes_to_move.append((box_r, box_c))
                box_r += dr
                box_c += dc
            if (box_r, box_c) not in walls:
                for box in boxes_to_move:
                    boxes.remove(box)
                    boxes.append((box[0] + dr, box[1] + dc))
            else:
                can_move = False

        if can_move:
            robot = new_robot

        # print("Move:", move)
        # show(robot, boxes, walls)
        # print()


def gps(box):
    r, c = box
    return 100 * r + c


def part1(robot, boxes, walls, moves):
    simulate(robot, boxes, walls, moves)
    return sum(gps(box) for box in boxes)


class Tests(unittest.TestCase):
    pass


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]
    sys.argv = sys.argv[:1]  # strip args, unittest.main() doesn't like them

    filename = "sample.txt" if "-s" in flags or "--sample" in flags else "input.txt"
    filename = args[0] if args else filename
    is_sample = filename.startswith("sample")
    run_tests = "-t" in flags or "--test" in flags

    if run_tests:
        unittest.main(exit=True)

    def check(part, actual, expected=None):
        print(f"Part {part}{' (sample)' if is_sample else ''}: {actual} ", end="")
        if expected is None:
            print("❔")
        else:
            if actual != expected:
                print(f"≠ {expected} ❌")
                exit(1)
            print("✅")

    robot, boxes, walls, moves = parse(filename)
    p1 = part1(robot, boxes, walls, moves)
    p2 = None

    check(1, p1, 10092 if is_sample else 1485257)
    check(2, p2)
