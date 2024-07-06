package aoc

import kotlin.test.Test
import kotlin.test.assertEquals

class AppTest {
    val boxes = listOf(Box(1, 2, 3), Box(1, 1, 1))

    @Test fun `part1 returns the sum of the volumes`() {
        assertEquals(6 + 1, part1(boxes))
    }

    @Test fun `part2 returns the sum of the surface areas`() {
        val area1 = 2 + 2 + 3 + 3 + 6 + 6
        val area2 = 1 + 1 + 1 + 1 + 1 + 1
        assertEquals(area1 + area2, part2(boxes))
    }

    @Test fun `parseLines converts lines to boxes`() {
        val result = parseLines(listOf("1x2x3", "987x10x1"))
        assert(result == listOf(Box(1, 2, 3), Box(987, 10, 1)))
    }
}
