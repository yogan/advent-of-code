package aoc

import kotlin.test.Test
import kotlin.test.assertEquals

class AppTest {
    @Test fun `blink works for the example`() =
        assertEquals(mapOf(1L to 2L, 2024L to 1L,  0L to 1L,  9L to 2L, 2021976L to 1L),
               blink(mapOf(0L to 1L,    1L to 1L, 10L to 1L, 99L to 1L,     999L to 1L)))
}
