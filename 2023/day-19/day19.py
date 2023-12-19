import sys, unittest

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

def parse(name=filename):
    workflow_lines, part_lines = open(name).read().strip().split("\n\n")

    workflows = {}
    for line in workflow_lines.split("\n"):
        name, rules = line.split("{")
        rules = [Rule(*rule.split(":")) if ":" in rule else Rule(None, rule)
                 for rule in rules[:-1].split(",")]
        workflows[name] = rules

    parts = [parse_part(line[1:-1]) for line in part_lines.split("\n")]

    return workflows, parts

def parse_part(part_line):
    return {x[0]: int(x[2:]) for x in part_line.split(",")}

def find_accepted_parts(workflows,parts):
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
    part2 = None

    check(1, part1, 19114 if is_sample else 432427)
    check(2, part2)
