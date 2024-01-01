import sys, unittest

flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))
args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]
sys.argv = sys.argv[:1]  # strip args, unittest.main() doesn't like them

filename = "sample.txt" if "-s" in flags or "--sample" in flags else "input.txt"
filename = args[0] if args else filename
is_sample = filename.startswith("sample")
run_tests = "-t" in flags or "--test" in flags

def parse():
    return [[int(num) for num in line.strip().split("x")]
            for line in open(filename).readlines()]

def volume(dimensions):
    l, w, h = dimensions
    return l * w * h

class TestDayXX(unittest.TestCase):
    def test_volume(self):
        self.assertEqual(volume([2, 3, 4]), 24)

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
    if run_tests:
        unittest.main(exit=True)

    lines = parse()
    part1 = sum([volume(x) for x in lines])
    part2 = None

    check(1, part1, 9876 if is_sample else None)
    check(2, part2)
