package aoc

import java.io.File

enum class Receiver { BOT, OUT }

data class Op(
    val bot: Int,
    val low:  Receiver, val lowId:  Int,
    val high: Receiver, val highId: Int,
)

fun main(args: Array<String>) {
    val filename = args[0]
    val lines = File(args[0]).readLines()
    val (low, high) = if (filename == "input.txt") { Pair(17, 61) } else { Pair(2, 5) }

    println("Part 1: ${part1(lines, low, high)}")
}

fun parseLines(lines: List<String>): Pair<MutableMap<Int, MutableList<Int>>, List<Op>> {
    val bots = mutableMapOf<Int, MutableList<Int>>()
    val ops  = mutableListOf<Op>()

    fun parseReceiver(s: String): Receiver = when (s) {
        "bot"    -> Receiver.BOT
        "output" -> Receiver.OUT
        else     -> throw Error("Unknown receiver: $s")
    }

    lines.forEach {
        val words = it.split(" ")
        when (words[0]) {
            "value" -> {
                val bot  = words[5].toInt()
                val chip = words[1].toInt()
                bots.getOrPut(bot) { mutableListOf() }.add(chip)
            }
            "bot" -> {
                val bot    = words[1].toInt()
                val low    = parseReceiver(words[5])
                val lowId  = words[6].toInt()
                val high   = parseReceiver(words[10])
                val highId = words[11].toInt()
                ops.add(Op(bot, low, lowId, high, highId))
            }
        }
    }

    return Pair(bots, ops)
}

fun part1(lines: List<String>, targetLow: Int, targetHigh: Int): Int {
    val outs = mutableMapOf<Int, MutableList<Int>>()
    val (bots, ops) = parseLines(lines)

    while(true) {
        val (bot, chips) = bots.entries.find { it.value.size == 2 } ?: break
        val (low, high) = chips.sorted()

        if (low == targetLow && high == targetHigh) {
            return bot
        }

        val op = ops.find { it.bot == bot }!!
        bots[bot] = mutableListOf()

        when (op.low) {
            Receiver.BOT -> bots.getOrPut(op.lowId) { mutableListOf() }.add(low)
            Receiver.OUT -> outs.getOrPut(op.lowId) { mutableListOf() }.add(low)
        }

        when (op.high) {
            Receiver.BOT -> bots.getOrPut(op.highId) { mutableListOf() }.add(high)
            Receiver.OUT -> outs.getOrPut(op.highId) { mutableListOf() }.add(high)
        }
    }

    throw Error("No bot with two chips found")
}
