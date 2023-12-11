import sys

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they scare the unittest module
is_sample = filename == "sample.txt"

def parse(filename=filename):
    with open(filename) as f:
        return [list(x.strip()) for x in f.readlines()]

def expand_space(image):
    expanded = [row.copy() for row in image]

    for c in range(len(image[0])):
        all_empty = all(x[c] == '.' for x in image)
        for r, row in enumerate(image):
            expanded[r][c] = 'X' if all_empty else row[c]

    for r in range(len(image)):
        all_empty = all(x == '.' for x in image[r])
        if all_empty:
            for c in range(len(image[0])):
                expanded[r][c] = 'X'

    return expanded

def find_galaxies(image):
    galaxies = []
    for r, row in enumerate(image):
        for c, col in enumerate(row):
            if col == '#':
                galaxies.append((r, c))
    return galaxies

def shortest_path(image, start, end, factor):
    x_rows_crossed = 0
    for r in range(min(start[0], end[0]), max(start[0], end[0])):
        if image[r][0] == 'X':
            x_rows_crossed += 1

    x_cols_crossed = 0
    for c in range(min(start[1], end[1]), max(start[1], end[1])):
        if image[0][c] == 'X':
            x_cols_crossed += 1

    dist = abs(start[0] - end[0]) + abs(start[1] - end[1])

    return dist + (factor - 1) * (x_rows_crossed + x_cols_crossed)

def sum_distances(image, galaxies, factor):
    sum = 0

    for i in range(len(galaxies)):
        for j in range(i+1, len(galaxies)):
            sum += shortest_path(image, galaxies[i], galaxies[j], factor)

    return sum

def print_and_assert(part, expected, actual):
    print(f"Part {part}: {actual}{' (sample)' if is_sample else ''}")
    assert actual == expected, f"{part} was {actual}, expected {expected}"

if __name__ == '__main__':
    image = expand_space(parse())
    galaxies = find_galaxies(image)

    part1 = sum_distances(image, galaxies, 2)
    part2 = sum_distances(image, galaxies, 1_000_000)

    print_and_assert(1, 374 if is_sample else 9403026, part1)
    print_and_assert(2, 82000210 if is_sample else 543018317006, part2)
