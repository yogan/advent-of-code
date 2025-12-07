package aoc

import java.io.File

data class Box(val length: Int, val width: Int, val height: Int) {
    val volume: Int get() = length * width * height
}

fun part1(boxes: List<Box>) = boxes.sumOf { it.volume }


fun main(args: Array<String>) {
    val boxes = parseLines(File(args[0]).readLines())
    println(part1(boxes))
}

fun parseLines(lines: List<String>): List<Box> = lines.map(::parseBox)

private fun parseBox(line: String): Box {
    val (length, width, height) = line.split("x").map(String::toInt)
    return Box(length, width, height)
}

