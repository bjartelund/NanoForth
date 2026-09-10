package nanoforth

/** Stage 5: output words `.` and `.S`.
  *
  * Up to now `Forth` has been pure: no I/O, just a data stack threaded
  * through immutable values. Printing seems to break that — but rather
  * than reach for real `println`/stdout (which would force every test
  * to capture and restore a shared global stream), output is modeled
  * the same way the stack already is: as more state on `Forth`.
  *
  * This adds an `output: String` field, defaulting to empty, that words
  * append to. `Forth` stays a pure value — nothing touches the console
  * — and something *outside* `Forth` (a REPL loop, later) is
  * responsible for actually printing `output` to the screen. Tests just
  * assert on it directly, no capturing required.
  *
  * Why a single `String` rather than, say, `Vector[String]` of printed
  * lines: real Forth doesn't have "lines" of output — every word writes
  * into one continuous character stream, and `.` conventionally follows
  * a printed number with a trailing space (so "1 . 2 ." reads as
  * "1 2 ", not "12 "). A single accumulated `String` matches that
  * directly, with no invented notion of "one vector entry per word"
  * to keep consistent as more printing words get added later.
  *
  *   - `.`  pops the top value and appends its decimal string plus a
  *     trailing space to `output`.
  *   - `.S` is non-destructive: it appends the whole stack, bottom to
  *     top, space-separated with a trailing space, without popping
  *     anything (handy for inspecting state without losing it — the
  *     classic Forth debugging word). On an empty stack it appends
  *     nothing at all.
  */
class Stage5OutputSpec extends munit.FunSuite {

  test("a fresh interpreter has empty output") {
    assertEquals(Forth().output, "")
  }

  test(". pops the top value and appends its decimal form plus a trailing space") {
    val result = Forth().run("42 .")
    assertEquals(result.output, "42 ")
    assertEquals(result.stack, Vector.empty)
  }

  test(". only consumes the top of the stack, not the whole stack") {
    val result = Forth().run("1 2 .")
    assertEquals(result.output, "2 ")
    assertEquals(result.stack, Vector(1L))
  }

  test(". on an empty stack raises StackUnderflowException") {
    intercept[StackUnderflowException] {
      Forth().run(".")
    }
  }

  test("output accumulates across multiple . calls, in order, without running together") {
    assertEquals(Forth().run("1 . 2 .").output, "1 2 ")
  }

  test(".S prints the whole stack, bottom to top, without consuming it") {
    val result = Forth().run("1 2 3 .S")
    assertEquals(result.output, "1 2 3 ")
    assertEquals(result.stack, Vector(1L, 2L, 3L))
  }

  test(".S on an empty stack appends nothing, and never underflows") {
    val result = Forth().run(".S")
    assertEquals(result.output, "")
    assertEquals(result.stack, Vector.empty)
  }

  test(". and .S compose: printing the top, then inspecting what's left") {
    val result = Forth().run("1 2 3 . .S")
    assertEquals(result.output, "3 1 2 ")
    assertEquals(result.stack, Vector(1L, 2L))
  }
}
