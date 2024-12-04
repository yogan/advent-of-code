import 'package:aoc/aoc.dart';
import 'package:test/test.dart';

void main() {
  var sample = parseLines([
    'MMMSXXMASM',
    'MSAMXMSMSA',
    'AMXSXMAAMM',
    'MSAMASMSMX',
    'XMASAMXAMM',
    'XXAMMXXAMA',
    'SMSMSASXSS',
    'SAXAMASAAA',
    'MAMMMXMMMM',
    'MXMXAXMASX',
  ]);

  test('part1', () {
    expect(part1(sample), 18);
  });

  test('part2', () {
    expect(part2(sample), 9);
  });
}
