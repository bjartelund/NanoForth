package nanoforth

/** Stage 7: conditionals, `IF ... THEN` and `IF ... ELSE ... THEN`.
  *
  * `IF` pops a flag and, like real Forth, treats any nonzero value as
  * true (not just -1) — the same convention already used by `AND`/`OR`.
  * If the flag is true, the tokens between `IF` and `ELSE` (or `THEN`,
  * if there's no `ELSE`) run; otherwise the tokens between `ELSE` and
  * `THEN` run (or nothing, if there's no `ELSE`). Either way, execution
  * resumes normally with whatever comes after `THEN`.
  *
  * Like `:`/`;` in stage 6, this can't be handled by `eval` one token
  * at a time — it needs to scan ahead for the matching `ELSE`/`THEN`,
  * the same way `runTokens` already scans ahead for `;`.
  *
  * This also means: for `IF` to work *inside* a word definition (which
  * is the main place it's actually useful), a defined word's body needs
  * to be replayed through that same scanning logic — i.e. via
  * `runTokens`, not a flat token-by-token `eval` fold. If your stage 6
  * implementation folds `eval` over the body directly, the `ABS`
  * test below is where that will need to change.
  *
  * Nested `IF` (one `IF` inside another `IF`'s branch) is out of scope
  * for this nano-Forth and isn't tested here.
  */
class Stage7ConditionalsSpec extends munit.FunSuite {

  test("a true (nonzero) flag runs the IF branch, with no ELSE") {
    assertEquals(Forth().run("-1 IF 42 THEN").stack, Vector(42L))
    assertEquals(Forth().run("1 IF 42 THEN").stack, Vector(42L))
  }

  test("a false (zero) flag skips the IF branch, with no ELSE") {
    assertEquals(Forth().run("0 IF 42 THEN").stack, Vector.empty)
  }

  test("execution resumes normally after THEN") {
    assertEquals(Forth().run("0 IF 42 THEN 7").stack, Vector(7L))
    assertEquals(Forth().run("1 IF 42 THEN 7").stack, Vector(42L, 7L))
  }

  test("a true flag runs the IF branch when ELSE is present") {
    assertEquals(Forth().run("1 IF 42 ELSE 99 THEN").stack, Vector(42L))
  }

  test("a false flag runs the ELSE branch when ELSE is present") {
    assertEquals(Forth().run("0 IF 42 ELSE 99 THEN").stack, Vector(99L))
  }

  test("IF composes with a comparison producing the flag") {
    assertEquals(Forth().run("3 4 < IF 100 ELSE 200 THEN").stack, Vector(100L))
    assertEquals(Forth().run("5 3 < IF 100 ELSE 200 THEN").stack, Vector(200L))
  }

  test("IF pops its flag off the stack") {
    assertEquals(Forth().run("9 1 IF THEN").stack, Vector(9L))
  }

  test("IF on an empty stack raises StackUnderflowException") {
    intercept[StackUnderflowException] {
      Forth().run("IF 42 THEN")
    }
  }

  test("IF without a matching THEN raises UnterminatedConditionalException") {
    intercept[UnterminatedConditionalException] {
      Forth().run("1 IF 42")
    }
  }

  test("IF/ELSE/THEN works inside a word definition") {
    val forth = Forth().run(": ABS DUP 0 < IF -1 * THEN ;")
    assertEquals(forth.run("5 ABS").stack, Vector(5L))
    assertEquals(forth.run("-5 ABS").stack, Vector(5L))
  }
}
