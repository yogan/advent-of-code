import std/[strutils, os]
import lib

let firstRow = readFile(paramStr(1)).strip()
echo safeTiles(firstRow, 40)
echo safeTiles(firstRow, 400000)
