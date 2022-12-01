from input import read_and_solve


def parse_input(lines):
    result = []
    for line in lines:
        heights = [int(digit) for digit in line]
        result.append(heights)
    return result


def get_neighbors(height_map, row, column, row_max, column_max):
    result = []
    if row > 0:
        result.append(height_map[row-1][column])
    if row < row_max:
        result.append(height_map[row+1][column])
    if column > 0:
        result.append(height_map[row][column-1])
    if column < column_max:
        result.append(height_map[row][column+1])
    return result


def calculate_low_points(height_map):
    low_points = []
    row_max = len(height_map)
    column_max = len(height_map[0])
    for row in range(row_max):
        for column in range(column_max):
            risk_level = height_map[row][column]
            neighbors = get_neighbors(
                height_map, row, column, row_max-1, column_max-1)
            larger_neighbors = [x for x in neighbors if x > risk_level]
            if len(neighbors) == len(larger_neighbors):
                low_points.append((row, column))

    return low_points


def add_if_candidate_fits(basin, candidate, current, row, column):
    if candidate >= current and candidate != 9:
        basin.add((row, column))


def calculate_basin_size(height_map, row, column):
    basin = {(row, column)}
    row_max = len(height_map)-1
    column_max = len(height_map[0])-1
    while True:
        next_basin = set()
        for row, column in basin:
            next_basin.add((row, column))
            current = height_map[row][column]
            if row > 0:
                candidate = height_map[row-1][column]
                add_if_candidate_fits(
                    next_basin, candidate, current, row-1, column)
            if row < row_max:
                candidate = height_map[row+1][column]
                add_if_candidate_fits(
                    next_basin, candidate, current, row+1, column)
            if column > 0:
                candidate = height_map[row][column-1]
                add_if_candidate_fits(
                    next_basin, candidate, current, row, column-1)
            if column < column_max:
                candidate = height_map[row][column+1]
                add_if_candidate_fits(
                    next_basin, candidate, current, row, column+1)
        if len(next_basin) == len(basin):
            return len(basin)
        basin = next_basin


def part1(lines):
    height_map = parse_input(lines)
    low_points = calculate_low_points(height_map)
    sum_of_risk_levels = sum(
        [height_map[row][column]+1 for row, column in low_points])

    return sum_of_risk_levels


def part2(lines):
    height_map = parse_input(lines)
    low_points = calculate_low_points(height_map)
    basin_sizes = []
    for row, column in low_points:
        basin_sizes.append(calculate_basin_size(height_map, row, column))
    basin_sizes.sort(reverse=True)
    return basin_sizes[0] * basin_sizes[1] * basin_sizes[2]


read_and_solve(__file__, part1, part2)
