package aoc

import java.io.File

data class Box(val l: Int, val w: Int, val h: Int)

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    val boxes = parseLines(lines)

    println("Part 1: ${part1(boxes)}")
    println("Part 2: ${part2(boxes)}")
}

fun parseLines(lines: List<String>): List<Box> =
    lines.map {
        val (l, w, h) = it.split("x").map { it.toInt() }
        Box(l, w, h)
    }

fun part1(boxes: List<Box>) = boxes.sumOf { volume(it) }

fun part2(boxes: List<Box>) = boxes.sumOf { surfaceArea(it) }

private fun volume(box: Box) = with(box) { l * w * h }

private fun surfaceArea(box: Box) = with(box) {
    val lw = l * w
    val wh = w * h
    val hl = h * l

    2 * (lw + wh + hl)
}
