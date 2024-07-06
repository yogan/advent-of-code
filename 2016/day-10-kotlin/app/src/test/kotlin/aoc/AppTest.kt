package aoc

import kotlin.test.Test
import kotlin.test.assertEquals

class AppTest {
    val lines = listOf(
        "value 5 goes to bot 2",
        "bot 2 gives low to bot 1 and high to bot 0",
        "value 3 goes to bot 1",
        "bot 1 gives low to output 1 and high to bot 0",
        "bot 0 gives low to output 2 and high to output 0",
        "value 2 goes to bot 2"
    )

    @Test fun `parseLines reads assignments and instructions`() {
        val (bots, instructions) = parseLines(lines)

        assertEquals(mapOf(
            1 to mutableListOf(3),
            2 to mutableListOf(5, 2),
        ), bots)

        assertEquals(listOf(
            Op(2, Receiver.BOT, 1, Receiver.BOT, 0),
            Op(1, Receiver.OUT, 1, Receiver.BOT, 0),
            Op(0, Receiver.OUT, 2, Receiver.OUT, 0),
        ), instructions)
    }

    @Test fun `balanceBots returns part 1 and part 2 sample results`() {
        val (part1, part2) = balanceBots(lines, 2, 5)

        assertEquals( 2, part1)
        assertEquals(30, part2)
    }
}
