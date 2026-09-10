package nanoforth

/** Stage 6: user-defined words, `: NAME ... ;`.
  *
  * `:` starts a definition, the next token is the new word's name, and
  * every token after that — up to the matching `;` — is the word's
  * body. Defining a word does nothing to the stack by itself; it only
  * takes effect the next time the name is used, at which point its body
  * runs exactly as if it had been typed inline.
  *
  * This requires `dictionary` to become part of `Forth`'s state (so a
  * definition survives into later calls, the same way `stack` and
  * `output` already do), and a new exception —
  * `UnterminatedDefinitionException` — for a `:` that never finds its
  * closing `;`.
  */
class Stage6WordDefinitionsSpec extends munit.FunSuite {

  test("defining a word does not itself change the stack") {
    assertEquals(Forth().run(": THREE 3 ;").stack, Vector.empty)
  }

  test("a defined word with no arguments pushes its body's result") {
    assertEquals(Forth().run(": THREE 3 ; THREE").stack, Vector(3L))
  }

  test("a defined word can use existing primitives, like DUP and *") {
    assertEquals(Forth().run(": SQUARE DUP * ; 5 SQUARE").stack, Vector(25L))
  }

  test("a defined word can be used more than once") {
    assertEquals(Forth().run(": SQUARE DUP * ; 2 SQUARE 3 SQUARE").stack, Vector(4L, 9L))
  }

  test("a defined word can call another previously-defined word") {
    val result = Forth().run(": SQUARE DUP * ; : QUAD SQUARE SQUARE ; 2 QUAD")
    assertEquals(result.stack, Vector(16L))
  }

  test("redefining a word replaces its old behavior") {
    val result = Forth().run(": DOUBLE DUP + ; : DOUBLE DUP DUP + + ; 3 DOUBLE")
    assertEquals(result.stack, Vector(9L))
  }

  test("a defined word can use output words like .") {
    assertEquals(Forth().run(": GREET 42 . ; GREET").output, "42 ")
  }

  test("a word body isn't checked until the word is actually called") {
    // NOPE isn't defined anywhere, but defining BROKEN to use it should
    // not fail — only calling BROKEN should.
    val defined = Forth().run(": BROKEN NOPE ;")
    assertEquals(defined.stack, Vector.empty)

    intercept[NoSuchElementException] {
      defined.run("BROKEN")
    }
  }

  test("a definition missing its closing ; raises UnterminatedDefinitionException") {
    intercept[UnterminatedDefinitionException] {
      Forth().run(": FOO 1 2 +")
    }
  }
}
