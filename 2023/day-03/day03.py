import sys

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
is_sample = filename == "sample.txt"

def read_lines():
    with open(filename) as f:
        lines = f.readlines()
    return [line.strip() for line in lines]

def find_part_numbers(lines):
    part_numbers = []

    for row, line in enumerate(lines):
        cur_num_str = ""
        start_col = None
        end_col = None
        for col, char in enumerate(line):
            if char.isdigit():
                cur_num_str += char
                if start_col is None:
                    start_col = col
                end_col = col
                if col == len(line) - 1:
                    part_numbers.append((int(cur_num_str), row, start_col, end_col))
                    cur_num_str = ""
                    start_col = None
                    end_col = None
            else:
                if cur_num_str != "":
                    part_numbers.append((int(cur_num_str), row, start_col, end_col))
                    cur_num_str = ""
                    start_col = None
                    end_col = None
                pass

    return part_numbers

def get_surrounding_chars(lines, row, start_col, end_col):
    surrounding_chars = ""

    for r in range(row-1, row+2):
        for c in range(start_col-1, end_col+2):
            if r == row and c >= start_col and c <= end_col:
                continue
            if r < 0 or r >= len(lines) or c < 0 or c >= len(lines[r]):
                continue
            char = lines[r][c]
            if char != "." and not char.isdigit():
                surrounding_chars += char

    return surrounding_chars

def find_gear(lines, row, start_col, end_col):
    for r in range(row-1, row+2):
        for c in range(start_col-1, end_col+2):
            if r == row and c >= start_col and c <= end_col:
                continue
            if r < 0 or r >= len(lines) or c < 0 or c >= len(lines[r]):
                continue
            if lines[r][c] == "*":
                return (r, c)

    return None

def get_gear_ratios(lines, part_numbers):
    ratios = []
    candidates = {}

    for part_number in part_numbers:
        gear = find_gear(lines, part_number[1], part_number[2], part_number[3])
        if gear:
            if candidates.get(gear):
                candidates[gear].append(part_number[0])
            else:
                candidates[gear] = [part_number[0]]

    for gear in candidates:
        if len(candidates[gear]) == 2:
            ratios.append((candidates[gear][0] * candidates[gear][1]))

    return ratios

def part1(lines, part_numbers):
    relevant_parts = []
    for part_number in part_numbers:
        surrounding_chars = get_surrounding_chars(lines, part_number[1], part_number[2], part_number[3])
        if surrounding_chars:
            relevant_parts.append(part_number)
    relevant_part_numbers = [part_number[0] for part_number in relevant_parts]
    return sum(relevant_part_numbers)

def part2(lines, part_numbers):
    gear_ratios = get_gear_ratios(lines, part_numbers)
    return sum(gear_ratios)

if __name__ == '__main__':
    lines = read_lines()
    part_numbers = find_part_numbers(lines)

    res1 = part1(lines, part_numbers)
    assert res1 == (4361 if is_sample else 507214)
    print(f"Part 1: {res1}{' (sample)' if is_sample else ''}")

    res2 = part2(lines, part_numbers)
    assert res2 == (467835 if is_sample else 72553319)
    print(f"Part 2: {res2}{' (sample)' if is_sample else ''}")
