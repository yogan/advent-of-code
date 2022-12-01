from typing import List, Dict
import unittest
from input import read_and_solve

segments_per_digit = {
    0: 6,
    1: 2,  # unique
    2: 5,
    3: 5,
    4: 4,  # unique
    5: 5,
    6: 6,
    7: 3,  # unique
    8: 7,  # unique
    9: 6,
}


def parse_input(lines):
    result = []
    for line in lines:
        line = line.strip()
        left, right = line.split(" | ")
        left_seqs = left.split(" ")
        right_seqs = right.split(" ")
        result.append([left_seqs, right_seqs])
    return result


def extract_right_seqs(seqs):
    return list(map(lambda s: s[1], seqs))


def count_unique_digits(seqs):
    occurrence = {
        0: 0,
        1: len(list(filter(lambda s: len(s) == segments_per_digit[1], seqs))),
        2: 0,
        3: 0,
        4: len(list(filter(lambda s: len(s) == segments_per_digit[4], seqs))),
        5: 0,
        6: 0,
        7: len(list(filter(lambda s: len(s) == segments_per_digit[7], seqs))),
        8: len(list(filter(lambda s: len(s) == segments_per_digit[8], seqs))),
        9: 0,
    }
    return sum(occurrence.values())


def part1(lines):
    seq_pairs = parse_input(lines) # [[left_seq_1, right_seqs_1], [ls2, rs2], …]
    right_seqs = extract_right_seqs(seq_pairs) # [right_seq_1, rs2, …]
    acc = 0
    for seq in right_seqs:
        acc += count_unique_digits(seq)
    return acc


def part2(lines):
    return 0


sample_lines = [
    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
    "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
    "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
    "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
    "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
    "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
    "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
    "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
    "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
    "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce",
]


class TestDay8(unittest.TestCase):

    def test_parse_input(self):
        result = parse_input(sample_lines[:1])
        self.assertEqual(result,
                         [
                             [
                                 ["be", "cfbegad", "cbdgef", "fgaecd", "cgeb",
                                  "fdcge", "agebfd", "fecdb", "fabcd", "edb"],
                                 ["fdgacbe", "cefdb", "cefbgd", "gcbe"],
                             ]
                         ])

    def test_extract_right_seqs(self):
        result = extract_right_seqs(
            [
                [
                    ["be", "cfbegad", "cbdgef", "fgaecd", "cgeb",
                     "fdcge", "agebfd", "fecdb", "fabcd", "edb"],
                    ["fdgacbe", "cefdb", "cefbgd", "gcbe"],
                ]
            ])
        self.assertEqual(result,
                         [
                             ["fdgacbe", "cefdb", "cefbgd", "gcbe"],
                         ])

    def test_count_unique_digits(self):
        result = count_unique_digits(["fdgacbe", "cefdb", "cefbgd", "gcbe"])
        self.assertEqual(result, 2)

    # def test_part_1_sample(self):
    #     self.assertEqual(part1(self.sample_lines), 37)

    # def test_part_2_sample(self):
    #     self.assertEqual(part2(self.sample_lines), 168)


if __name__ == '__main__':
    unittest.main(exit=False)
    print()
    read_and_solve(__file__, part1, part2)
