from input import read_and_solve


def calc_most_common_bit(numbers, index):
    most_common_bit = 0

    for number in numbers:
        line_bits = list(number)
        bit = line_bits[index]
        if bit == '0':
            most_common_bit -= 1
        else:
            most_common_bit += 1

    return most_common_bit


def part1(lines):
    digits = len(lines[0]) - 1

    gamma_bits = [0] * digits
    epsilon_bits = [0] * digits

    for i in range(digits):
        if calc_most_common_bit(lines, i) > 0:
            gamma_bits[i] = "1"
            epsilon_bits[i] = "0"
        else:
            gamma_bits[i] = "0"
            epsilon_bits[i] = "1"

    gamma = int("".join(gamma_bits), 2)
    epsilon = int("".join(epsilon_bits), 2)

    return gamma * epsilon


def has_at_index(digits, value, index):
    return digits[index] == value


def reduce_lines_oxygen(lines, i):
    most_common_bit = calc_most_common_bit(lines, i)
    if most_common_bit >= 0:
        return list(filter(lambda line: has_at_index(line, "1", i), lines))
    else:
        return list(filter(lambda line: has_at_index(line, "0", i), lines))


def reduce_lines_co2(lines, i):
    most_common_bit = calc_most_common_bit(lines, i)
    if most_common_bit < 0:
        return list(filter(lambda line: has_at_index(line, "1", i), lines))
    else:
        return list(filter(lambda line: has_at_index(line, "0", i), lines))


def part2(lines):
    index = 0
    oxygen_lines = lines
    while len(oxygen_lines) > 1:
        oxygen_lines = reduce_lines_oxygen(oxygen_lines, index)
        index += 1

    oxygen_generator_rating = int(oxygen_lines[0], 2)

    index = 0
    co2_lines = lines
    while len(co2_lines) > 1:
        co2_lines = reduce_lines_co2(co2_lines, index)
        index += 1

    co2_scrubber_rating = int(co2_lines[0], 2)

    return oxygen_generator_rating * co2_scrubber_rating


read_and_solve(__file__, part1, part2)
