import unittest
from input import read_and_solve

parens = {
    '(': ')',
    '[': ']',
    '{': '}',
    '<': '>',
}

score_for_illegal_char = {
    ')': 3,
    ']': 57,
    '}': 1197,
    '>': 25137,
}

score_for_unclosed_paren = {
    '(': 1,
    '[': 2,
    '{': 3,
    '<': 4,
}


def score_for_stack(stack):
    score = 0
    for paren in reversed(stack):
        score *= 5
        score += score_for_unclosed_paren[paren]
    return score


def map_stack_to_matching_chars(stack):
    return [parens[opener] for opener in reversed(stack)]


def syntax_check(line):
    stack = []
    for char in line:
        if char in "([{<":
            stack.append(char)
        else:
            expected = stack.pop()
            if char != parens[expected]:
                return (char, stack)
    return (None, stack)


def part1(lines):
    illegal_chars = [syntax_check(line)[0] for line in lines]
    scores = [score_for_illegal_char[char] for char in illegal_chars if char]
    return sum(scores)


def part2(lines):
    checked_lines = [syntax_check(line) for line in lines]
    stacks = [line[1] for line in checked_lines if line[0] is None]
    scores = [score_for_stack(stack) for stack in stacks]
    scores.sort()
    return scores[len(scores)//2]


class TestDay7(unittest.TestCase):

    sample_lines = [
        "[({(<(())[]>[[{[]{<()<>>",
        "[(()[<>])]({[<{<<[]>>(",
        "{([(<{}[<>[]}>{[]{[(<()>",
        "(((({<>}<{<{<>}{[]{[]{}",
        "[[<[([]))<([[{}[[()]]]",
        "[{[{({}]{}}([{[{{{}}([]",
        "{<[[]]>}<{[{[{[]{()[[[]",
        "[<(<(<(<{}))><([]([]()",
        "<{([([[(<>()){}]>(<<{{",
        "<{([{{}}[<[[[<>{}]]]>[]]",
    ]

    def test_get_first_illegal_char_incomplete(self):
        line = "[({(<(())[]>[[{[]{<()<>>"
        char, stack = syntax_check(line)
        expected_stack = [*"[({([[{{"]
        self.assertEqual(char, None)
        self.assertEqual(stack, expected_stack)

    def test_get_first_illegal_char_illegal(self):
        line = "{([(<{}[<>[]}>{[]{[(<()>"
        char, _ = syntax_check(line)
        self.assertEqual(char, '}')

    def test_map_stack_to_matching_chars(self):
        stack = [*"[({([[{{"]
        expected = [*"}}]])})]"]
        result = map_stack_to_matching_chars(stack)
        self.assertEqual(result, expected)

    def test_part_1_sample(self):
        self.assertEqual(part1(self.sample_lines), 26397)

    def test_part_2_sample(self):
        self.assertEqual(part2(self.sample_lines), 288957)


if __name__ == '__main__':
    unittest.main(exit=False)
    read_and_solve(__file__, part1, part2)
