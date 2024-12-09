import sys
import unittest


def part1(disk_map):
    blocks = gen_blocks(disk_map)
    defrag(blocks)
    return checksum(blocks)


def gen_blocks(disk_map):
    blocks = []
    is_file = True
    id = 0
    for c in disk_map:
        for _ in range(int(c)):
            blocks.append(str(id) if is_file else ".")
        if is_file:
            id += 1
        is_file = not is_file
    return blocks


def defrag(blocks):
    target = blocks.index(".")
    source = len(blocks) - 1
    while target < source:
        blocks[target], blocks[source] = blocks[source], blocks[target]
        target += 1
        while blocks[target] != ".":
            target += 1
        source -= 1
        while blocks[source] == ".":
            source -= 1


def checksum(blocks):
    result = 0
    for pos, id in enumerate(blocks):
        if id == ".":
            break
        result += pos * int(id)
    return result


class Tests(unittest.TestCase):
    def test_gen_blocks(self):
        self.assertEqual(gen_blocks("12345"), list("0..111....22222"))
        self.assertEqual(
            gen_blocks("2333133121414131402"),
            list("00...111...2...333.44.5555.6666.777.888899"),
        )

    def test_defrag(self):
        blocks = list("0..111....22222")
        defrag(blocks)
        self.assertEqual(blocks, list("022111222......"))

        blocks = list("00...111...2...333.44.5555.6666.777.888899")
        defrag(blocks)
        self.assertEqual(blocks, list("0099811188827773336446555566.............."))

    def test_checksum(self):
        self.assertEqual(
            checksum(list("0099811188827773336446555566..............")), 1928
        )


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]
    sys.argv = sys.argv[:1]  # strip args, unittest.main() doesn't like them

    filename = "sample.txt" if "-s" in flags or "--sample" in flags else "input.txt"
    filename = args[0] if args else filename
    is_sample = filename.startswith("sample")
    run_tests = "-t" in flags or "--test" in flags

    if run_tests:
        unittest.main(exit=True)

    def check(part, actual, expected=None):
        print(f"Part {part}{' (sample)' if is_sample else ''}: {actual} ", end="")
        if expected is None:
            print("❔")
        else:
            if actual != expected:
                print(f"≠ {expected} ❌")
                exit(1)
            print("✅")

    disk_map = open(filename).read().strip()
    p1 = part1(disk_map)
    p2 = None

    check(1, p1, 1928 if is_sample else 6448989155953)
    check(2, p2)
