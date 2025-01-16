package aoc

import java.io.File

fun main(args: Array<String>) {
    val stones = parse(args[0])

    println(blinkblink(stones, 25))
    println(blinkblink(stones, 75))
}

private fun parse(filename: String): Map<Long, Long> =
    File(filename).readText().trim().split(" ")
        .groupingBy { it.toLong() }
        .eachCount()
        .mapValues { it.value.toLong() }

private fun blinkblink(stones: Map<Long, Long>, times: Int): Long =
    (1..times).fold(stones) { acc, _ -> blink(acc) }.values.sum()

fun blink(stones: Map<Long, Long>): Map<Long, Long> {
    val newStones = mutableMapOf<Long, Long>()

    for ((stone, factor) in stones)
        for (s in evolve(stone))
            newStones[s] = newStones.getOrDefault(s, 0) + factor

    return newStones
}

private fun evolve(stone: Long): List<Long> {
    if (stone == 0L) return listOf(1)

    val stoneStr = stone.toString()

    return if (stoneStr.length % 2 == 0) {
        val mid = stoneStr.length / 2
        listOf(stoneStr.substring(0, mid).toLong(), stoneStr.substring(mid).toLong())
    } else {
        listOf(stone * 2024)
    }
}
