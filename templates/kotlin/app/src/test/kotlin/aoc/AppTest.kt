package aoc

import kotlin.test.Test
import kotlin.test.assertNotNull

class AppTest {
    @Test fun appHasAGreeting() {
        val classUnderTest = App()
        assertNotNull(classUnderTest.greeting, "app should have a greeting")
    }

    @Test fun greetingIsHelloWorld() {
        val classUnderTest = App()
        assert(classUnderTest.greeting == "Hello World!")
    }
}
