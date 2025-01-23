import std/os
import lib

let data = readFile(paramStr(1))
let robots = parseInput(data)
echo part1(robots)
echo part2(robots)
