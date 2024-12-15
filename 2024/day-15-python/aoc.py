import sys


def parse(filename, wide=False):
    map, moves = open(filename).read().strip().split("\n\n")
    robot = None
    boxes = []
    walls = []
    factor = 2 if wide else 1

    for r, row in enumerate(map.split("\n")):
        for c, cell in enumerate(row):
            if cell == "@":
                robot = (r, c * factor)
            elif cell == "O":
                if wide:
                    boxes.append(((r, c * factor), (r, c * factor + 1)))
                else:
                    boxes.append((r, c))
            elif cell == "#":
                walls.append((r, c * factor))
                if wide:
                    walls.append((r, c * factor + 1))

    return robot, boxes, walls, moves.replace("\n", "")


def show(robot, boxes, walls):
    max_r, max_c = walls[-1]
    for r in range(max_r + 1):
        for c in range(max_c + 1):
            if (r, c) == robot:
                print("@", end="")
            elif (r, c) in boxes:
                print("O", end="")
            elif ((r, c - 1), (r, c)) in boxes:
                print("]", end="")
            elif ((r, c), (r, c + 1)) in boxes:
                print("[", end="")
            elif (r, c) in walls:
                print("#", end="")
            else:
                print(".", end="")
        print()


def simulate(robot, boxes, walls, moves, wide=False):
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

        new_robot = (robot[0] + dr, robot[1] + dc)

        if new_robot in walls:
            continue

        can_move = True
        r, c = new_robot

        if wide:
            boxes_to_move = []

            if move in "<>":
                if move == ">":
                    while ((r, c), (r, c + 1)) in boxes:
                        boxes_to_move.append(((r, c), (r, c + 1)))
                        c += 2
                else:
                    while ((r, c - 1), (r, c)) in boxes:
                        boxes_to_move.append(((r, c - 1), (r, c)))
                        c -= 2
                if (r, c) not in walls:
                    for box in boxes_to_move:
                        boxes.remove(box)
                        (_, lc), (_, rc) = box
                        boxes.append(((r, lc + dc), (r, rc + dc)))
                else:
                    can_move = False

            else:
                left = right = c
                while True:
                    if is_wall_in_range(walls, r, left, right):
                        can_move = False
                        break
                    bs = boxes_in_range(boxes, r, left, right)
                    if not bs:
                        break
                    for b in bs:
                        left = min(c for (_, c), _ in bs)
                        right = max(c for _, (_, c) in bs)
                        boxes_to_move.append(b)
                    r += dr
                if can_move:
                    for box in boxes_to_move:
                        boxes.remove(box)
                        (lr, lc), (rr, rc) = box
                        boxes.append(((lr + dr, lc), (rr + dr, rc)))

        elif new_robot in boxes:
            boxes_to_move = []
            while (r, c) in boxes:
                boxes_to_move.append((r, c))
                r += dr
                c += dc
            if (r, c) not in walls:
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


def boxes_in_range(boxes, r, left, right):
    result = []
    for c in range(left, right + 2):
        if ((r, c - 1), (r, c)) in boxes:
            result.append(((r, c - 1), (r, c)))
    return result


def is_wall_in_range(walls, r, left, right):
    for c in range(left, right + 1):
        if (r, c) in walls:
            return True
    return False


def gps(box, wide=False):
    if wide:
        (r, c), _ = box
    else:
        r, c = box
    return 100 * r + c


def run(robot, boxes, walls, moves, wide=False):
    # print("Initial state:")
    # show(robot, boxes, walls)
    # print()
    simulate(robot, boxes, walls, moves, wide)
    return sum(gps(box, wide) for box in boxes)


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]

    filename = "sample.txt" if "-s" in flags or "--sample" in flags else "input.txt"
    filename = args[0] if args else filename
    is_sample = filename.startswith("sample")

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
    p1 = run(robot, boxes, walls, moves)

    robot, boxes, walls, moves = parse(filename, wide=True)
    p2 = run(robot, boxes, walls, moves, wide=True)

    check(1, p1, 10092 if is_sample else 1485257)
    check(2, p2, 9021 if is_sample else 1475512)
