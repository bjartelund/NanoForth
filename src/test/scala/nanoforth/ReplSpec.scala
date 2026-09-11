package nanoforth

/** `Repl.step` is the pure core of the REPL: no I/O, just
  * (current state, one line of input) => (next state, text to show).
  */
class ReplSpec extends munit.FunSuite {

  test("a line with no printing words shows just ok") {
    val (result, shown) = Repl.step(Forth(), "3 4 +")
    assertEquals(result.stack, Vector(7L))
    assertEquals(shown, "ok")
  }

  test("a line that prints shows its output before ok") {
    val (_, shown) = Repl.step(Forth(), "3 4 + .")
    assertEquals(shown, "7 ok")
  }

  test("only the current line's output is shown, not the whole session's") {
    val (afterFirst, firstShown) = Repl.step(Forth(), "1 .")
    val (_, secondShown) = Repl.step(afterFirst, "2 .")
    assertEquals(firstShown, "1 ok")
    assertEquals(secondShown, "2 ok")
  }

  test("state carries forward across successful steps") {
    val (afterDefine, _) = Repl.step(Forth(), ": SQUARE DUP * ;")
    val (afterUse, shown) = Repl.step(afterDefine, "5 SQUARE")
    assertEquals(afterUse.stack, Vector(25L))
    assertEquals(shown, "ok")
  }

  test("a failing line reports an error and leaves the state unchanged") {
    val before = Forth().run("3")
    val (after, shown) = Repl.step(before, "+")
    assertEquals(after, before)
    assert(shown.startsWith("Error"), s"expected an error message, got: $shown")
  }

  test("an unknown word is reported as an error, not thrown") {
    val (after, shown) = Repl.step(Forth(), "FROBNICATE")
    assertEquals(after, Forth())
    assert(shown.startsWith("Error"), s"expected an error message, got: $shown")
  }

  test("division by zero is reported as an error, not thrown") {
    val (after, shown) = Repl.step(Forth(), "5 0 /")
    assertEquals(after, Forth())
    assert(shown.startsWith("Error"), s"expected an error message, got: $shown")
  }

  test("the session can continue normally after an error") {
    val (afterError, _) = Repl.step(Forth(), "+")
    val (afterRecovery, shown) = Repl.step(afterError, "3 4 +")
    assertEquals(afterRecovery.stack, Vector(7L))
    assertEquals(shown, "ok")
  }
}
