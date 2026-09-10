package nanoforth

/** Stage 1: pushing numbers onto the stack.
  *
  * In Forth, anything that isn't a recognized word is parsed as a number
  * and pushed onto the data stack. This is the smallest useful slice of
  * a Forth interpreter, and everything else (arithmetic, stack words,
  * word definitions) will be built on top of `Forth.eval` / `Forth.run`.
  *
  * `stack` is bottom-to-top, so after running "1 2 3" the top of the
  * stack (3) is the *last* element of the Vector.
  */
class Stage1NumbersSpec extends munit.FunSuite {

  test("running empty input leaves the stack empty") {
    assertEquals(Forth().run("").stack, Vector.empty)
  }

  test("running blank input (just whitespace) leaves the stack empty") {
    assertEquals(Forth().run("   ").stack, Vector.empty)
  }

  test("a single number is pushed onto the stack") {
    assertEquals(Forth().run("42").stack, Vector(42L))
  }

  test("multiple numbers are pushed in order, left to right") {
    assertEquals(Forth().run("1 2 3").stack, Vector(1L, 2L, 3L))
  }

  test("extra whitespace between numbers is ignored") {
    assertEquals(Forth().run("  1   2  3 ").stack, Vector(1L, 2L, 3L))
  }

  test("negative numbers are pushed correctly") {
    assertEquals(Forth().run("-5").stack, Vector(-5L))
  }

  test("running starts from the interpreter's existing stack, not a fresh one") {
    val start = Forth(Vector(1L, 2L))
    assertEquals(start.run("3").stack, Vector(1L, 2L, 3L))
  }

  test("eval pushes a single number, same as a one-token run") {
    assertEquals(Forth().eval("7"), Forth().run("7"))
  }

  test("a word that isn't a number raises an error") {
    intercept[NoSuchElementException] {
      Forth().run("frobnicate")
    }
  }
}
