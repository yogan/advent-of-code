import sys

if len(sys.argv) != 2:
    print("Missing input file.")
    exit(1)
filename  = sys.argv[1]
is_sample = filename == "sample.txt"

def parse():
    return [line.strip() for line in open(filename).readlines()]

def travel(garden):
    R, C = len(garden), len(garden[0])

    plots = set([(r, c) for r in range(R) for c in range(C)
                 if garden[r][c] == 'S'])

    for _ in range(64):
        next = set()
        for pos in plots:
            r, c = pos
            for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
                rr, cc = r + dr, c + dc
                if 0 <= rr < R and 0 <= cc < C and garden[rr][cc] != '#':
                    next.add((rr, cc))
        plots = next

    return len(plots)

def check(part, actual, expected=None):
    print(f"Part {part}{' (sample)' if is_sample else ''}: {actual} ", end="")
    if expected is None:
        print("❔")
    else:
        if actual != expected:
            print(f"≠ {expected} ❌")
            exit(1)
        print("✅")

if __name__ == '__main__':
    garden = parse()

    part1 = travel(garden)
    part2 = None

    check(1, part1, 42 if is_sample else 3816)
    check(2, part2)
