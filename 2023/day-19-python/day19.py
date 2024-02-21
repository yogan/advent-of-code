import sys, unittest
from collections import deque

if len(sys.argv) != 2:
    print("Missing input file.")
    exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they confuse the unittest module
is_sample = filename == "sample.txt"

class Rule:
    def __init__(self, condition, target):
        self.target = target

        if condition is None:
            self.category  = None
            self.compare   = None
            self.value     = None
        else:
            assert condition[0] in "xmas", f"Invalid category: {condition[0]}"
            assert condition[1] in "<>", f"Invalid comparison: {condition[1]}"
            self.category = condition[0]
            self.compare  = condition[1]
            self.value    = int(condition[2:])

    def __repr__(self):
        if self.category is None:
            return f"Rule(→{self.target})"
        return f"Rule({self.category}{self.compare}{self.value}→{self.target})"

    def __eq__(self, other):
        return self.target   == other.target   and \
               self.category == other.category and \
               self.compare  == other.compare  and \
               self.value    == other.value

    def apply(self, xl, xh, ml, mh, al, ah, sl, sh):
        if self.compare == ">":
            if self.category == "x":
                xl = max(self.value + 1, xl)
            elif self.category == "m":
                ml = max(self.value + 1, ml)
            elif self.category == "a":
                al = max(self.value + 1, al)
            elif self.category == "s":
                sl = max(self.value + 1, sl)
        elif self.compare == "<":
            if self.category == "x":
                xh = min(self.value - 1, xh)
            elif self.category == "m":
                mh = min(self.value - 1, mh)
            elif self.category == "a":
                ah = min(self.value - 1, ah)
            elif self.category == "s":
                sh = min(self.value - 1, sh)

        return xl, xh, ml, mh, al, ah, sl, sh

    def apply_inverse(self, xl, xh, ml, mh, al, ah, sl, sh):
        if self.compare == ">":
            # treat as <=
            if self.category == "x":
                xh = min(self.value, xh)
            elif self.category == "m":
                mh = min(self.value, mh)
            elif self.category == "a":
                ah = min(self.value, ah)
            elif self.category == "s":
                sh = min(self.value, sh)
        elif self.compare == "<":
            # treat as >=
            if self.category == "x":
                xl = max(self.value, xl)
            elif self.category == "m":
                ml = max(self.value, ml)
            elif self.category == "a":
                al = max(self.value, al)
            elif self.category == "s":
                sl = max(self.value, sl)

        return xl, xh, ml, mh, al, ah, sl, sh

def parse(name=filename):
    workflow_lines, part_lines = open(name).read().strip().split("\n\n")

    workflows = {}
    for line in workflow_lines.split("\n"):
        name, rules = line.split(chr(123))
        rules = [Rule(*rule.split(":")) if ":" in rule else Rule(None, rule)
                 for rule in rules[:-1].split(",")]
        workflows[name] = rules

    parts = [parse_part(line[1:-1]) for line in part_lines.split("\n")]

    return workflows, parts

def parse_part(part_line):
    return {x[0]: int(x[2:]) for x in part_line.split(",")}

def find_accepted_parts(workflows, parts):
    accepted = []

    for part in parts:
        rules = workflows["in"].copy()
        while rules:
            rule = rules.pop(0)
            if rule.category is None or \
                    (rule.compare == "<" and part[rule.category] < rule.value) or \
                    (rule.compare == ">" and part[rule.category] > rule.value):
                if rule.target == "A":
                    accepted.append(part)
                    rules.clear()
                elif rule.target == "R":
                    rules.clear()
                else:
                    rules = workflows[rule.target].copy()

    return accepted

def traverse(workflows):
    combs = 0
    queue = deque([("in", 1, 4000, 1, 4000, 1, 4000, 1, 4000)])

    while queue:
        cur, xl, xh, ml, mh, al, ah, sl, sh = queue.popleft()

        if cur == "R":
            continue

        if cur == "A":
            combs += (xh-xl+1) * (mh-ml+1) * (ah-al+1) * (sh-sl+1)
            continue

        for rule in workflows[cur]:
            queue.append((rule.target,
                         *rule.apply(xl, xh, ml, mh, al, ah, sl, sh)))
            xl, xh, ml, mh, al, ah, sl, sh = \
                rule.apply_inverse(xl, xh, ml, mh, al, ah, sl, sh)

    return combs

class TestDay19(unittest.TestCase):
    def test_parse_sample(self):
        workflows, parts = parse("sample.txt")

        self.assertEqual(len(workflows), 11)
        self.assertEqual(workflows["px"],
                         [Rule('a<2006', 'qkq'), Rule('m>2090', 'A'), Rule(None, 'rfg')])
        self.assertEqual(workflows["in"],
                         [Rule('s<1351', 'px'), Rule(None, 'qqz')])
        self.assertEqual(workflows["hdj"],
                         [Rule('m>838', 'A'), Rule(None, 'pv')])

        self.assertEqual(len(parts),  5)
        self.assertEqual(parts[0],
                         {'x': 787, 'm': 2655, 'a': 1222, 's': 2876})
        self.assertEqual(parts[-1],
                         {'x': 2127, 'm': 1623, 'a': 2188, 's': 1013})

def check(part, actual, expected=None):
    print(f"Part {part}{' (sample)' if is_sample else ''}: {actual} ", end="")
    if expected is None:
        print("❔")
    else:
        if actual != expected:
            print(f"≠ {expected} ❌")
            exit(1)
        print("✅")

if __name__ == '__main__':
    unittest.main(exit=False)
    print()

    workflows, parts = parse()
    accepted = find_accepted_parts(workflows, parts)

    part1 = sum(sum(part.values()) for part in accepted)
    part2 = traverse(workflows)

    check(1, part1, 19114 if is_sample else 432427)
    check(2, part2, 167409079868000 if is_sample else 143760172569135)
