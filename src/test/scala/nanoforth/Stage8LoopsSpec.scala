package nanoforth

/** Stage 8: counted loops, `DO ... LOOP`, and the loop-index word `I`.
  *
  * `limit start DO ... LOOP` runs the body once for every index from
  * `start` (inclusive) up to `limit` (exclusive), incrementing by one
  * each time. `DO` pops its two operands the same way every other
  * binary word does — the top of the stack (pushed last) is `start`,
  * the value below it is `limit` — so "5 0 DO" counts 0, 1, 2, 3, 4.
  *
  * `I`, used inside the loop body, pushes whatever the current index
  * is. Unlike every earlier word, its result changes on every
  * iteration — it isn't reading the data stack, it's reading the loop's
  * own running state.
  *
  * A `DO` nested in a loop body is rejected with `NestedLoopException`
  * (and a `:` in the body, with `NestedDefinitionException`), because
  * the single-level parse would misread the nested markers, and a
  * nested `:` would redefine a word on every iteration.
  */
class Stage8LoopsSpec extends munit.FunSuite {

  test("DO/LOOP runs the body once per index, with I pushing the current index") {
    assertEquals(Forth().run("5 0 DO I . LOOP").output, "0 1 2 3 4 ")
  }

  test("a loop from start to limit with start == limit runs zero times") {
    val result = Forth().run("0 0 DO I . LOOP")
    assertEquals(result.output, "")
    assertEquals(result.stack, Vector.empty)
  }

  test("the loop body can compute with I, not just print it") {
    assertEquals(Forth().run("3 0 DO I 2 * . LOOP").output, "0 2 4 ")
  }

  test("values pushed inside the loop accumulate on the stack across iterations") {
    assertEquals(Forth().run("100 3 0 DO I LOOP").stack, Vector(100L, 0L, 1L, 2L))
  }

  test("DO leaves values below its own operands untouched") {
    assertEquals(Forth().run("100 2 0 DO LOOP").stack, Vector(100L))
  }

  test("DO on a stack with fewer than two values raises StackUnderflowException") {
    intercept[StackUnderflowException] {
      Forth().run("DO I LOOP")
    }
  }

  test("a DO without a matching LOOP raises UnterminatedLoopException") {
    intercept[UnterminatedLoopException] {
      Forth().run("5 0 DO I .")
    }
  }

  test("execution resumes normally after LOOP") {
    assertEquals(Forth().run("2 0 DO I LOOP 99").stack, Vector(0L, 1L, 99L))
  }

  test("DO/LOOP works inside a word definition") {
    val forth = Forth().run(": COUNTUP 0 DO I . LOOP ;")
    assertEquals(forth.run("5 COUNTUP").output, "0 1 2 3 4 ")
    assertEquals(forth.run("3 COUNTUP").output, "0 1 2 ")
  }

  test("a nested DO in a loop body raises NestedLoopException") {
    intercept[NestedLoopException] {
      Forth().run("5 0 DO 3 0 DO 1 LOOP LOOP")
    }
  }

  test("a : in a loop body raises NestedDefinitionException") {
    intercept[NestedDefinitionException] {
      Forth().run("5 0 DO : F 42 ; LOOP")
    }
  }
}
