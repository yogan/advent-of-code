package aoc

import kotlin.test.Test
import kotlin.test.assertEquals

class AppTest {
    @Test
    fun `columns parses start of sample`() {
        val input = listOf(
            ".......S.......",
            "...............",
            ".......^.......",
            "...............",
            "......^.^......"
        )
        val expected = listOf(
            listOf(7),
            listOf(7),
            listOf(6, 8)
        )
        assertEquals(expected, columns(input))
    }

    @Test
    fun `travelMultiverses returns sample results`() {
        val rows = listOf(
            listOf(7),
            listOf(7),
            listOf(6, 8),
            listOf(5, 7, 9),
            listOf(4, 6, 10),
            listOf(3, 5, 9, 11),
            listOf(2, 6, 12),
            listOf(1, 3, 5, 7, 9, 13)
        )
        assertEquals(21 to 40L, travelMultiverses(rows))
    }
}
