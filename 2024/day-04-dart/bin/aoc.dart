import 'dart:io';
import 'package:aoc/aoc.dart';

void main(List<String> arguments) async {
  var lines = await File(arguments[0]).readAsLines();
  var letters = parseLines(lines);

  print(part1(letters));
  print(part2(letters));
}
