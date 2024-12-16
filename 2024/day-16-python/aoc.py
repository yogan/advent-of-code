import sys
from queue import PriorityQueue


def parse(filename):
    maze = [line.strip() for line in open(filename).readlines()]
    rows, cols = len(maze), len(maze[0])
    start, end = None, None
    nodes = set()
    for r in range(1, rows):
        for c in range(1, cols):
            if maze[r][c] == "S":
                start = (r, c)
            elif maze[r][c] == "E":
                end = (r, c)
            if maze[r][c] != "#":
                nodes.add((r, c))
    return nodes, start, end


def shortest_path(nodes, start, end):
    rotations = {
        "E": ["N", "S"],
        "S": ["E", "W"],
        "W": ["N", "S"],
        "N": ["E", "W"],
    }

    moves = {
        "E": (0, 1),
        "S": (1, 0),
        "W": (0, -1),
        "N": (-1, 0),
    }

    distances = {}
    for node in nodes:
        for dir in "ESWN":
            distances[(node, dir)] = float("inf")
    distances[(start, "E")] = 0

    queue = PriorityQueue()
    queue.put((0, start, "E"))

    while not queue.empty():
        dist, cur, dir = queue.get()

        if cur == end:
            return dist

        dr, dc = moves[dir]
        next = (cur[0] + dr, cur[1] + dc)
        if next in nodes:
            new_dist = dist + 1
            if new_dist < distances[(next, dir)]:
                distances[(next, dir)] = new_dist
                queue.put((new_dist, next, dir))

        for new_dir in rotations[dir]:
            new_dist = dist + 1000
            if new_dist < distances[(cur, new_dir)]:
                distances[(cur, new_dir)] = new_dist
                queue.put((new_dist, cur, new_dir))


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

    nodes, start, end = parse(filename)
    part1 = shortest_path(nodes, start, end)
    part2 = None

    check(1, part1, 7036 if is_sample else 72400)
    check(2, part2)
