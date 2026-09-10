package nanoforth

/** Stage 4: comparisons `= < >` and logic `AND OR`.
  *
  * Forth doesn't have a separate boolean type — a "flag" is just a
  * number. The convention (which nano-Forth follows) is:
  *   - true  is -1 (every bit set)
  *   - false is  0 (no bits set)
  *
  * That's not an arbitrary choice: with true = all-ones, the bitwise
  * `AND`/`OR` you'd want for numbers *are* logical and/or on flags, so
  * one pair of words does both jobs. (This is also why it's -1 and not
  * 1 — 1 is just "0...001", and `AND`-ing two arbitrary true flags
  * together wouldn't reliably stay true unless every true value shares
  * every bit, which only "all bits set" guarantees.)
  *
  * `= < >` follow the same operand order as the arithmetic words:
  * "a b op" tests `a op b`.
  */
class Stage4ComparisonSpec extends munit.FunSuite {

  test("= pushes -1 (true) when the top two values are equal") {
    assertEquals(Forth().run("3 3 =").stack, Vector(-1L))
  }

  test("= pushes 0 (false) when the top two values differ") {
    assertEquals(Forth().run("3 4 =").stack, Vector(0L))
  }

  test("< pushes -1 (true) when a < b, for \"a b <\"") {
    assertEquals(Forth().run("3 4 <").stack, Vector(-1L))
  }

  test("< pushes 0 (false) when a is not less than b") {
    assertEquals(Forth().run("4 3 <").stack, Vector(0L))
    assertEquals(Forth().run("3 3 <").stack, Vector(0L))
  }

  test("> pushes -1 (true) when a > b, for \"a b >\"") {
    assertEquals(Forth().run("5 3 >").stack, Vector(-1L))
  }

  test("> pushes 0 (false) when a is not greater than b") {
    assertEquals(Forth().run("3 5 >").stack, Vector(0L))
    assertEquals(Forth().run("3 3 >").stack, Vector(0L))
  }

  test("AND is bitwise, so -1 AND -1 is true and anything AND 0 is false") {
    assertEquals(Forth().run("-1 -1 AND").stack, Vector(-1L))
    assertEquals(Forth().run("-1 0 AND").stack, Vector(0L))
    assertEquals(Forth().run("0 0 AND").stack, Vector(0L))
  }

  test("OR is bitwise, so any -1 makes the result true") {
    assertEquals(Forth().run("-1 0 OR").stack, Vector(-1L))
    assertEquals(Forth().run("0 0 OR").stack, Vector(0L))
    assertEquals(Forth().run("-1 -1 OR").stack, Vector(-1L))
  }

  test("comparisons compose: combining two true comparisons with AND") {
    // 3 < 4 is true (-1), 5 < 6 is true (-1); -1 AND -1 is true.
    assertEquals(Forth().run("3 4 < 5 6 < AND").stack, Vector(-1L))
  }

  test("comparisons compose: combining a false comparison with OR") {
    // 3 > 4 is false (0), 5 < 6 is true (-1); 0 OR -1 is true.
    assertEquals(Forth().run("3 4 > 5 6 < OR").stack, Vector(-1L))
  }

  test("each comparison/logic word raises StackUnderflowException when starved") {
    for word <- List("=", "<", ">", "AND", "OR") do
      intercept[StackUnderflowException] {
        Forth().run(s"1 $word")
      }
  }
}
