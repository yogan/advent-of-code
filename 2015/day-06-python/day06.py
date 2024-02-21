def part1():
    grid = [[False] * 1000 for _ in range(1000)]

    for line in open("input.txt").readlines():
        parts = line.split(" ")
        if parts[0] == "turn":
            start = [int(num) for num in parts[2].split(",")]
            end = [int(num) for num in parts[4].split(",")]
            for x in range(start[0], end[0] + 1):
                for y in range(start[1], end[1] + 1):
                    grid[x][y] = parts[1] == "on"
        elif parts[0] == "toggle":
            start = [int(num) for num in parts[1].split(",")]
            end = [int(num) for num in parts[3].split(",")]
            for x in range(start[0], end[0] + 1):
                for y in range(start[1], end[1] + 1):
                    grid[x][y] = not grid[x][y]

    return sum([row.count(True) for row in grid])


def part2():
    grid = [[0] * 1000 for _ in range(1000)]

    for line in open("input.txt").readlines():
        parts = line.split(" ")
        if parts[0] == "turn":
            start = [int(num) for num in parts[2].split(",")]
            end = [int(num) for num in parts[4].split(",")]
            for x in range(start[0], end[0] + 1):
                for y in range(start[1], end[1] + 1):
                    if parts[1] == "on":
                        grid[x][y] += 1
                    elif parts[1] == "off":
                        grid[x][y] = max(0, grid[x][y] - 1)
        elif parts[0] == "toggle":
            start = [int(num) for num in parts[1].split(",")]
            end = [int(num) for num in parts[3].split(",")]
            for x in range(start[0], end[0] + 1):
                for y in range(start[1], end[1] + 1):
                    grid[x][y] += 2

    return sum([sum(row) for row in grid])


def check(part, actual, expected=None):
    print(f"Part {part}: {actual} ", end="")
    if expected is None:
        print("❔")
    else:
        if actual != expected:
            print(f"≠ {expected} ❌")
            exit(1)
        print("✅")


if __name__ == "__main__":
    check(1, part1(), 569999)
    check(2, part2(), 17836115)
