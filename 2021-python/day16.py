import unittest
import math
from input import read_and_solve

# LITERAL VALUE PACKET
#
#              7     E     5
#           0111  1110  0101
#           |  |  |  |  |  |
# 110 100  10111 11110 00101  000
#  |   |               |       `----- padding (0-3 0s)
#  |   |               `------------- 0 = last seq of 5
#  |   `----------------------------- version 110 = 6
#  `--------------------------------- type id 100 = 4 = literal


def parse_input(line):
    num = int(line, 16)
    binary_digits = list(bin(num))[2:]  # [2:] as string looks like "0b0101…"

    # bin() does not give us leading 0s, so we have to take care of that:
    first_hex_digit = int(line[0], 16)
    if first_hex_digit == 0:
        binary_digits.insert(0, "0")
        binary_digits.insert(0, "0")
        binary_digits.insert(0, "0")
        binary_digits.insert(0, "0")
    elif first_hex_digit == 1:
        binary_digits.insert(0, "0")
        binary_digits.insert(0, "0")
        binary_digits.insert(0, "0")
    elif first_hex_digit in [2, 3]:
        binary_digits.insert(0, "0")
        binary_digits.insert(0, "0")
    elif first_hex_digit in [4, 5, 6, 7]:
        binary_digits.insert(0, "0")

    return binary_digits


def bits_to_decimal(bits):
    return int("".join(bits), 2)


def parse_version_numbers(bits):
    i = 0
    i_max = len(bits) - 1
    version_numbers = []

    parse(bits, i, i_max, version_numbers)

    return version_numbers


def parse_literal(bits, i):
    length = 6  # for padding to % 4, whole packet (/w header) counts

    # Take five bits at a time until leading zero is found:
    while True:
        five_bits = bits[i:i+5]
        i += 5
        length += 5
        if five_bits[0] == "0":
            break

    # Trailing padding 0s for literal shit
    # Possible lengths are 5 * n: 5, 10, 15, …
    # Padded length has to be multiple of four. 5 -> 8 (3), 10 - 12 (2)
    # WTF is this forumula? Might be right, tho…
    padding_zeroes = (4 - (length % 4)) % 4
    assert (length + padding_zeroes) % 4 == 0  # I have trust issues
    i += padding_zeroes

    # print("length", length)
    # print("padded zeros:", padding_zeroes)
    # print("returning from parse_literal with i = ", i - 1)

    return i - 1  # WHYYYYY ლ(ಠ益ಠლ)


def parse_operator(bits, i, version_numbers):
    length_type_id = bits[i]
    i += 1

    if length_type_id == "0":
        # Next 15 bits represent the total length of the following
        # sub-packets (NOT including version/type header or the length
        # stuff itself!)
        sub_packet_length = bits_to_decimal(bits[i:i+15])
        i += 15
        print("SUB PACKET! /w len:", sub_packet_length)
        return parse(bits, i, i + sub_packet_length - 1, version_numbers)

    else:
        # Next 11 bits represent the number (NOT length!) of following
        # sub-packets
        num_of_sub_packets = bits_to_decimal(bits[i:i+11])
        i += 11
        print("SUB PACKET! # of sub-packets:", num_of_sub_packets)
        return parse(bits, i, math.inf, version_numbers, num_of_sub_packets)


def parse(bits, i, i_max, version_numbers, max_packets=math.inf):
    print("parse(), i = ", i, "i_max = ", i_max, "versions", version_numbers,
          "max packets:", max_packets)
    # if i_max is math.inf:
    #     print("".join(bits[i:]))
    # else:
    #     print("".join(bits[i:i_max+1]))

    assert i_max is math.inf or i_max < len(bits)

    # minimal packet probably 11 bits (3+3 header + 5 literal bits)
    # while i < i_max - 11:
    while i <= i_max and max_packets > 0:
        version_bits = bits[i:i+3]
        i += 3
        if i > i_max or i > len(bits) - 5:  # magic offset of the very end
            break

        type_id_bits = bits[i:i+3]
        i += 3
        if i > i_max or i > len(bits) - 5:  # magic offset of the very end
            break

        version_numbers.append(bits_to_decimal(version_bits))
        max_packets -= 1

        if type_id_bits == list("100"):
            # print("literal packet, i (header start) = ", i - 6)
            i = parse_literal(bits, i)
        else:
            print("operator packet, i (header start) = ", i - 6)
            i = parse_operator(bits, i, version_numbers)

    return i


def part1(lines):
    bits = parse_input(lines[0])
    print("# of bits:", len(bits))
    version_numbers = parse_version_numbers(bits)
    return sum(version_numbers)


def part2(lines):
    return 0


class TestDay16(unittest.TestCase):

    # real input, just so that we also have it available in tests w/o file I/O
    real_input = "".join([
        "C20D59802D2B0B6713C6B4D1600ACE7E3C179BFE391E546CC017F004A4F513C9",
        "D973A1B2F32C3004E6F9546D005840188C51DA298803F1863C42160068E5E377",
        "59BC4908C0109E76B00425E2C530DE40233CA9DE8022200EC618B10DC001098E",
        "F0A63910010D3843350C6D9A252805D2D7D7BAE1257FD95A6E928214B66DBE69",
        "1E0E9005F7C00BC4BD22D733B0399979DA7E34A6850802809A1F9C4A947B9157",
        "9C063005B001CF95B77504896A884F73D7EBB900641400E7CDFD56573E941E67",
        "EABC600B4C014C829802D400BCC9FA3A339B1C9A671005E35477200A0A551E80",
        "15591F93C8FC9E4D188018692429B0F930630070401B8A90663100021313E1C4",
        "7900042A2B46C840600A580213681368726DEA008CEDAD8DD5A6181801460070",
        "801CE0068014602005A011ECA0069801C200718010C0302300AA2C02538007E2",
        "C01A100052AC00F210026AC0041492F4ADEFEF7337AAF2003AB360B23B3398F0",
        "09005113B25FD004E5A32369C068C72B0C8AA804F0AE7E36519F6296D76509DE",
        "70D8C2801134F84015560034931C8044C7201F02A2A180258010D4D4E347D92A",
        "F6B35B93E6B9D7D0013B4C01D8611960E9803F0FA2145320043608C4284C4016",
        "CE802F2988D8725311B0D443700AA7A9A399EFD33CD5082484272BC9E67C984C",
        "F639A4D600BDE79EA462B5372871166AB33E001682557E5B74A0C49E25AACE76",
        "D074E7C5A6FD5CE697DC195C01993DCFC1D2A032BAA5C84C012B004C001098FD",
        "1FE2D00021B0821A45397350007F66F021291E8E4B89C118FE40180F802935CC",
        "12CD730492D5E2B180250F7401791B18CCFBBCD818007CB08A664C7373CEEF9F",
        "D05A73B98D7892402405802E000854788B91BC0010A861092124C2198023C019",
        "8880371222FC3E100662B45B8DB236C0F080172DD1C300820BCD1F4C24C8AAB0",
        "015F33D280",
    ])

    # cat inputs/16/yogan.txt | tr -d '\n' | wc - -c
    real_input_length = 1354

    # first sample packet: literal value
    sample_literal_hex = "D2FE28"
    sample_literal_bin = list("110100101111111000101000")

    # second sample packet: operator /w I=0
    sample_operator_1_hex = "38006F45291200"
    sample_operator_1_bin = list(
        # i =                 22         33             48     55
        #                      |          |              |      |
        "00111000000000000110111101000101001010010001001000000000")
    #    VVVTTTILLLLLLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBBBBBB
    #                          VVV        VVV
    #    001                   110        010
    # version numbers: [0b001, 0b110, 0b010] = [1, 6, 2]
    sample_operator_1_version_numbers = [1, 6, 2]

    # third sample packet: operator /w I=1
    sample_operator_2_hex = "EE00D40C823060"
    sample_operator_2_bin = list(
        "11101110000000001101010000001100100000100011000001100000")
    #    VVVTTTILLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBCCCCCCCCCCC
    #                      VVV        VVV        VVV
    #    111               010        100        001
    # version numbers: [0b111, 0b010, 0b100, 0b001] = [7, 2, 4, 1]
    sample_operator_2_version_numbers = [7, 2, 4, 1]

    # just to check that the weird string multiline stuff above actually works
    def test_input(self):
        self.assertEqual(len(self.real_input), self.real_input_length)

    def test_parse_input_real_input(self):
        bits = parse_input(self.real_input)

        # Checking the full binary representation would be nuts, but let's do at
        # least a few sanity checks, to see if can actually turn that lengthy
        # hex string into an even longer binary string.
        # First, a length check: each hex digit turns into four binary digits:
        bits_per_hex_digit = 4
        self.assertEqual(len(bits), self.real_input_length *
                         bits_per_hex_digit)

        # Start of input is "C2…", 0xC = 0b1100, 0x2 = 0b0010, so "1100 0010":
        self.assertEqual(self.real_input[:2], "C2")
        self.assertEqual(bits[:2 * bits_per_hex_digit], list("1100" "0010"))

        # End of input is "…80", 0x8 = 0b0100, 0x0 = 0b0000, so "1000 0000":
        self.assertEqual(self.real_input[-2:], "80")
        self.assertEqual(bits[-2 * bits_per_hex_digit:], list("1000" "0000"))

    def test_parse_input_leading_zeroes(self):
        self.assertEqual(parse_input("0A"), list("0000" "1010"))
        self.assertEqual(parse_input("1A"), list("0001" "1010"))
        self.assertEqual(parse_input("2A"), list("0010" "1010"))
        self.assertEqual(parse_input("3A"), list("0011" "1010"))
        self.assertEqual(parse_input("4A"), list("0100" "1010"))
        self.assertEqual(parse_input("5A"), list("0101" "1010"))
        self.assertEqual(parse_input("6A"), list("0110" "1010"))
        self.assertEqual(parse_input("7A"), list("0111" "1010"))
        self.assertEqual(parse_input("8A"), list("1000" "1010"))
        self.assertEqual(parse_input("9A"), list("1001" "1010"))
        self.assertEqual(parse_input("AA"), list("1010" "1010"))
        self.assertEqual(parse_input("BA"), list("1011" "1010"))
        self.assertEqual(parse_input("CA"), list("1100" "1010"))
        self.assertEqual(parse_input("DA"), list("1101" "1010"))
        self.assertEqual(parse_input("EA"), list("1110" "1010"))
        self.assertEqual(parse_input("FA"), list("1111" "1010"))

    def test_parse_input_sample_literal_value(self):
        bits = parse_input(self.sample_literal_hex)
        self.assertEqual(bits, self.sample_literal_bin)

    def test_parse_input_sample_operator_1(self):
        bits = parse_input(self.sample_operator_1_hex)
        self.assertEqual(bits, self.sample_operator_1_bin)

    def test_parse_input_sample_operator_2(self):
        bits = parse_input(self.sample_operator_2_hex)
        self.assertEqual(bits, self.sample_operator_2_bin)

    def test_bits_to_decimal(self):
        self.assertEqual(bits_to_decimal(list("0000")),  0)
        self.assertEqual(bits_to_decimal(list("0001")),  1)
        self.assertEqual(bits_to_decimal(list("0010")),  2)
        self.assertEqual(bits_to_decimal(list("0011")),  3)
        self.assertEqual(bits_to_decimal(list("0100")),  4)
        self.assertEqual(bits_to_decimal(list("0101")),  5)
        self.assertEqual(bits_to_decimal(list("0110")),  6)
        self.assertEqual(bits_to_decimal(list("0111")),  7)
        self.assertEqual(bits_to_decimal(list("1000")),  8)
        self.assertEqual(bits_to_decimal(list("1001")),  9)
        self.assertEqual(bits_to_decimal(list("1010")), 10)
        self.assertEqual(bits_to_decimal(list("1011")), 11)
        self.assertEqual(bits_to_decimal(list("1100")), 12)
        self.assertEqual(bits_to_decimal(list("1101")), 13)
        self.assertEqual(bits_to_decimal(list("1110")), 14)
        self.assertEqual(bits_to_decimal(list("1111")), 15)

    def test_parse_version_numbers_literal(self):
        versions = parse_version_numbers(self.sample_literal_bin)
        self.assertEqual(versions, [6])

    def test_parse_version_numbers_operator_1(self):
        versions = parse_version_numbers(self.sample_operator_1_bin)
        self.assertEqual(versions, self.sample_operator_1_version_numbers)

    def test_parse_version_numbers_operator_2(self):
        versions = parse_version_numbers(self.sample_operator_2_bin)
        self.assertEqual(versions, self.sample_operator_2_version_numbers)

    def test_part_1_sample_a(self):
        self.assertEqual(part1(["8A004A801A8002F478"]), 16)

    def test_part_1_sample_b(self):
        self.assertEqual(part1(["620080001611562C8802118E34"]), 12)

    def test_part_1_sample_c(self):
        self.assertEqual(part1(["C0015000016115A2E0802F182340"]), 23)

    def test_part_1_sample_d(self):
        self.assertEqual(part1(["A0016C880162017C3686B18A3D4780"]), 31)

    # def test_part_1_sample(self):
    #     self.assertEqual(part1(self.sample), TODO)

    # def test_part_2_sample(self):
    #     self.assertEqual(part2(self.sample), TODO)


if __name__ == '__main__':
    unittest.main(exit=False)
    read_and_solve(__file__, part1, part2)
