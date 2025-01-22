import std/os
import lib

let data = readFile(paramStr(1))
let boxes = parseInput(data)
echo part1(boxes)
echo part2(boxes)
