package aoc

import java.io.File

typealias Beams = MutableMap<Int, Long>

fun main(args: Array<String>) =
    args[0].run(::parse).run(::travelMultiverses).printResults()

fun travelMultiverses(rows: List<List<Int>>): Pair<Int, Long> {
    val beams = rows.first().associateWith { 1L }.toMutableMap()
    val splits = rows.drop(1).sumOf { beams.applySplitters(it) }

    return splits to beams.countTimelines()
}

private fun Beams.applySplitters(splitters: List<Int>): Int =
    this.toList()
        .filter { (col, _) -> col in splitters }
        .onEach { (col, t) -> this.splitTimelines(col, t) }
        .size

private fun Beams.splitTimelines(col: Int, timelines: Long) {
    this.addTimelines(col - 1, timelines)
    this.addTimelines(col + 1, timelines)
    this.remove(col)
}

private fun Beams.addTimelines(col: Int, timelines: Long) =
    this.merge(col, timelines, Long::plus)

private fun Beams.countTimelines() = values.sum()

private fun parse(filename: String): List<List<Int>> =
    filename.run(::File).readLines().run(::columns)

fun columns(lines: List<String>): List<List<Int>> =
    lines.filter { line -> line.any { it != '.' } }
        .map { line -> line.indices.filter { line[it] != '.' } }

private fun Pair<Int, Long>.printResults() = println("$first\n$second")
