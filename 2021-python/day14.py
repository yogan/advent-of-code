from collections import Counter, defaultdict
import unittest
from input import read_and_solve


def parse_input(lines):
    template_line, rule_lines = (lines[0], lines[2:])

    template = list(template_line)

    rule_pairs = [list(rule.split(" -> ")) for rule in rule_lines]
    rules = {key: value for key, value in rule_pairs}

    return template, rules


def parse_input_2(lines):
    template_line, rule_lines = (lines[0], lines[2:])

    rule_pairs = [list(rule.split(" -> ")) for rule in rule_lines]
    rules = {key: value for key, value in rule_pairs}

    chars = list(template_line)
    pairs = defaultdict(int)

    for i in range(len(chars) - 1):
        key = chars[i] + chars[i+1]
        pairs[key] += 1

    return pairs, rules


def step(template, rules):
    new_template = []

    for i in range(len(template) - 1):
        c1, c2 = template[i], template[i+1]
        to_insert = rules[c1 + c2]
        new_template.append(c1)
        new_template.append(to_insert)

    new_template.append(template[-1])

    return new_template


def step_2(pairs, char_counter, rules):
    new_pairs = defaultdict(int)

    for pair, count in pairs.items():
        middle = rules[pair]
        seq1 = pair[0] + middle
        seq2 = middle + pair[1]
        new_pairs[seq1] += count
        new_pairs[seq2] += count
        char_counter[middle] += count

    return new_pairs


def find_extremes(template):
    histogram = Counter(template)
    counts = histogram.values()
    return min(counts), max(counts)


def find_extremes_2(char_counter):
    counts = char_counter.values()
    return min(counts), max(counts)


def part1(lines):
    template, rules = parse_input(lines)

    for _ in range(10):
        template = step(template, rules)

    min, max = find_extremes(template)
    return max - min


def part2(lines):
    pairs, rules = parse_input_2(lines)

    char_counter = Counter(lines[0])

    for _ in range(40):
        pairs = step_2(pairs, char_counter, rules)

    min, max = find_extremes_2(char_counter)
    return max - min


class TestDay14(unittest.TestCase):

    sample = [
        "NNCB",
        "",
        "CH -> B",
        "HH -> N",
        "CB -> H",
        "NH -> C",
        "HB -> C",
        "HC -> B",
        "HN -> C",
        "NN -> C",
        "BH -> H",
        "NC -> B",
        "NB -> B",
        "BN -> B",
        "BB -> N",
        "BC -> B",
        "CC -> N",
        "CN -> C",
    ]

    def test_parse_input(self):
        template, rules = parse_input(self.sample)
        self.assertEqual(template, ['N', 'N', 'C', 'B'])
        self.assertEqual(rules, {
            "CH": "B",
            "HH": "N",
            "CB": "H",
            "NH": "C",
            "HB": "C",
            "HC": "B",
            "HN": "C",
            "NN": "C",
            "BH": "H",
            "NC": "B",
            "NB": "B",
            "BN": "B",
            "BB": "N",
            "BC": "B",
            "CC": "N",
            "CN": "C",
        })

    def test_parse_input_2(self):
        pairs, rules = parse_input_2(self.sample)
        self.assertEqual(pairs, {  # ['N', 'N', 'C', 'B'])
            "CB": 1,
            "NN": 1,
            "NC": 1,
        })
        self.assertEqual(rules, {
            "CH": "B",
            "HH": "N",
            "CB": "H",
            "NH": "C",
            "HB": "C",
            "HC": "B",
            "HN": "C",
            "NN": "C",
            "BH": "H",
            "NC": "B",
            "NB": "B",
            "BN": "B",
            "BB": "N",
            "BC": "B",
            "CC": "N",
            "CN": "C",
        })

    def test_step(self):
        result = step(list("NNCB"), {
            "CH": "B",
            "HH": "N",
            "CB": "H",
            "NH": "C",
            "HB": "C",
            "HC": "B",
            "HN": "C",
            "NN": "C",
            "BH": "H",
            "NC": "B",
            "NB": "B",
            "BN": "B",
            "BB": "N",
            "BC": "B",
            "CC": "N",
            "CN": "C",
        })
        self.assertEqual(result, list("NCNBCHB"))

    def test_step_2(self):
        pairs = defaultdict(int)
        pairs["CB"] = 1
        pairs["NN"] = 1
        pairs["NC"] = 1
        char_counter = Counter("NNCB")
        new_pairs = step_2(pairs, char_counter, {
            "CH": "B",
            "HH": "N",
            "CB": "H",
            "NH": "C",
            "HB": "C",
            "HC": "B",
            "HN": "C",
            "NN": "C",
            "BH": "H",
            "NC": "B",
            "NB": "B",
            "BN": "B",
            "BB": "N",
            "BC": "B",
            "CC": "N",
            "CN": "C",
        })
        self.assertEqual(new_pairs["BC"], 1)
        self.assertEqual(new_pairs["CH"], 1)
        self.assertEqual(new_pairs["CN"], 1)
        self.assertEqual(new_pairs["HB"], 1)
        self.assertEqual(new_pairs["NB"], 1)
        self.assertEqual(new_pairs["NC"], 1)
        self.assertEqual(char_counter["N"], 2)
        self.assertEqual(char_counter["C"], 2)
        self.assertEqual(char_counter["B"], 2)
        self.assertEqual(char_counter["H"], 1)

    def test_find_extremes(self):
        min, max = find_extremes("AAACAAABBDDDAAA")
        self.assertEqual(min, 1)
        self.assertEqual(max, 3 * 3)

    def test_part_1_sample(self):
        self.assertEqual(part1(self.sample), 1588)

    def test_part_2_sample(self):
        self.assertEqual(part2(self.sample), 2188189693529)


if __name__ == '__main__':
    unittest.main(exit=False)
    read_and_solve(__file__, part1, part2)
