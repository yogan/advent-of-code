import hashlib
import sys
import unittest
from functools import lru_cache


def find_index_64(salt, extra_rounds=0):
    index = 0
    keys = 0

    while True:
        t = triplet(hash(salt, index, extra_rounds))
        if t:
            for i in range(index + 1, index + 1001):
                if t * 5 in hash(salt, i, extra_rounds):
                    keys += 1
                    # print(f"Found key #{keys} at index {index}")
                    if keys == 64:
                        return index
        index += 1


# Stats (w/o extra rounds):
# - no caching:              hashcount: 2330805, runtime: 5.5 s
# - lru_cache(maxsize=999):  hashcount: 2330805, runtime: 6.7 s
# - lru_cache(maxsize=1000): hashcount:   23721, runtime: 0.5 s
# - lru_cache(maxsize=1001): hashcount:   23721, runtime: 0.5 s
@lru_cache(maxsize=1000)
def hash(salt, index, extra_rounds=0):
    h = hashlib.md5(f"{salt}{index}".encode()).hexdigest()
    for _ in range(extra_rounds):
        h = hashlib.md5(h.encode()).hexdigest()
    return h


def triplet(s):
    for i in range(len(s) - 2):
        if s[i] == s[i + 1] == s[i + 2]:
            return s[i]
    return None


class Tests(unittest.TestCase):
    def test_hash(self):
        self.assertIn("cc38887a5", hash("abc", 18))

    def test_extra_secure_hash(self):
        self.assertIn("a107ff", hash("abc", 0, extra_rounds=2016))

    def test_triplet(self):
        self.assertEqual(triplet("abc"), None)
        self.assertEqual(triplet("aaabc"), "a")
        self.assertEqual(triplet("baaac"), "a")
        self.assertEqual(triplet("bcaaa"), "a")
        self.assertEqual(triplet("aaaaaa"), "a")
        self.assertEqual(triplet("baaacaaa"), "a")
        self.assertEqual(triplet("bbbacaaa"), "b")
        self.assertEqual(triplet("xbbbaxxcccccaaa"), "b")


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]

    filename = "sample.txt" if "-s" in flags or "--sample" in flags else "input.txt"
    filename = args[0] if args else filename
    is_sample = filename.startswith("sample")
    run_tests = "-t" in flags or "--test" in flags

    def check(part, actual, expected=None):
        print(f"Part {part}{' (sample)' if is_sample else ''}: {actual} ", end="")
        if expected is None:
            print("❔")
        else:
            if actual != expected:
                print(f"≠ {expected} ❌")
                exit(1)
            print("✅")

    if run_tests:
        unittest.main(argv=sys.argv[:1])

    salt = open(filename).read().strip()
    part1 = find_index_64(salt)
    part2 = find_index_64(salt, extra_rounds=2016)

    check(1, part1, 22728 if is_sample else 15035)
    check(2, part2, 22551 if is_sample else 19968)
