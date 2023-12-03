import unittest, sys, re

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they scare the unittest module
is_sample = filename == "sample.txt"

def read_lines(filename=filename):
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
            # middle row, exclude sart_col to end_col
            if r == row and c >= start_col and c <= end_col:
                continue
            if r < 0 or r >= len(lines):
                continue
            if c < 0 or c >= len(lines[r]):
                continue
            char = lines[r][c]
            if char != "." and not char.isdigit():
                surrounding_chars += char

    return surrounding_chars

def find_surrounding_gears(lines, row, start_col, end_col):
    for r in range(row-1, row+2):
        for c in range(start_col-1, end_col+2):
            # middle row, exclude sart_col to end_col
            if r == row and c >= start_col and c <= end_col:
                continue
            if r < 0 or r >= len(lines):
                continue
            if c < 0 or c >= len(lines[r]):
                continue
            if lines[r][c] == "*":
                return (r, c)

    return None

def find_gear_ratios(lines, part_numbers):
    gear_rations = []
    candidates = {}

    for part_number in part_numbers:
        surrounding_gears = find_surrounding_gears(lines, part_number[1], part_number[2], part_number[3])
        if surrounding_gears:
            if candidates.get(surrounding_gears):
                candidates[surrounding_gears].append(part_number[0])
            else:
                candidates[surrounding_gears] = [part_number[0]]

    for candidate in candidates:
        # print(f"candidates[candidate]: {candidate} {candidates[candidate]}")
        if len(candidates[candidate]) == 2:
            gear_rations.append((candidates[candidate][0] * candidates[candidate][1]))

    return gear_rations

def part1():
    lines = read_lines()
    part_numbers = find_part_numbers(lines)
    relevant_parts = []
    for part_number in part_numbers:
        surrounding_chars = get_surrounding_chars(lines, part_number[1], part_number[2], part_number[3])
        # print(f"sorrounding_chars for num {part_number[0]}: {surrounding_chars}")
        if surrounding_chars:
            relevant_parts.append(part_number)
    # print(f"relevant_part_numbers: {relevant_parts}")
    relevant_part_numbers = [part_number[0] for part_number in relevant_parts]
    return sum(relevant_part_numbers)

def part2():
    lines = read_lines()
    part_numbers = find_part_numbers(lines)
    gear_ratios = find_gear_ratios(lines, part_numbers)
    return sum(gear_ratios)

if __name__ == '__main__':
    res1 = part1()
    assert res1 == (4361 if is_sample else 507214)
    print(f"Part 1: {res1}", "(sample)" if is_sample else "")
    res2 = part2()
    assert res2 == (467835 if is_sample else 72553319)
    print(f"Part 2: {res2}", "(sample)" if is_sample else "")
