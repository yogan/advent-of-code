package aoc

import java.io.File

data class Box(val l: Int, val w: Int, val h: Int)

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    val boxes = parseLines(lines)

    println("Part 1: ${part1(boxes)}")
    println("Part 2: ${part2(boxes)}")
}

fun parseLines(lines: List<String>): List<Box> {
    return lines.map {
        val (l, w, h) = it.split("x").map { it.toInt() }
        Box(l, w, h)
    }
}

fun surfaceArea(box: Box): Int {
    val lw = box.l * box.w
    val wh = box.w * box.h
    val hl = box.h * box.l
    return 2 * lw + 2 * wh + 2 * hl
}

fun volume(box: Box): Int {
    return box.l * box.w * box.h
}

fun part1(boxes: List<Box>): Int {
    return boxes.sumOf { volume(it) }
}

fun part2(box: List<Box>): Int {
    return box.sumOf { surfaceArea(it) }
}
