package aoc

import kotlin.test.Test
import kotlin.test.assertEquals

class AppTest {
    val boxes = listOf(Box(1, 2, 3), Box(1, 1, 1))

    @Test
    fun `part1 returns the sum of the volumes`() =
        assertEquals(7, part1(boxes))

    @Test
    fun `parseLines converts lines to boxes`() =
        assertEquals(
            listOf(Box(1, 2, 3), Box(987, 10, 1)),
            parseLines(listOf("1x2x3", "987x10x1"))
        )
}
