package nanoforth

/** Stage 2: arithmetic words `+ - * /`.
  *
  * Each of these is a binary word: it pops two numbers off the stack and
  * pushes one result. Given "a b op", the top of the stack (b) was
  * pushed last, so the operation is `a op b` — e.g. "5 3 -" means
  * 5 - 3, not 3 - 5. Get the operand order right or subtraction and
  * division will silently do the wrong thing.
  *
  * You'll need to introduce a `StackUnderflowException` (thrown when a
  * word needs more operands than the stack has) for these tests to
  * compile — a case class extending `RuntimeException`, carrying the
  * word that failed, is enough.
  */
class Stage2ArithmeticSpec extends munit.FunSuite {

  test("+ adds the top two numbers") {
    assertEquals(Forth().run("1 2 +").stack, Vector(3L))
  }

  test("- subtracts top from second-from-top (a b - => a - b)") {
    assertEquals(Forth().run("5 3 -").stack, Vector(2L))
  }

  test("* multiplies the top two numbers") {
    assertEquals(Forth().run("3 4 *").stack, Vector(12L))
  }

  test("/ divides second-from-top by top (a b / => a / b), truncating") {
    assertEquals(Forth().run("10 2 /").stack, Vector(5L))
    assertEquals(Forth().run("7 2 /").stack, Vector(3L))
  }

  test("dividing by zero raises ArithmeticException") {
    intercept[ArithmeticException] {
      Forth().run("5 0 /")
    }
  }

  test("arithmetic words leave untouched values below them on the stack") {
    assertEquals(Forth().run("100 1 2 +").stack, Vector(100L, 3L))
  }

  test("results can feed into further operations, left to right") {
    assertEquals(Forth().run("1 2 + 3 *").stack, Vector(9L))
  }

  test("+ on an empty stack raises StackUnderflowException") {
    intercept[StackUnderflowException] {
      Forth().run("+")
    }
  }

  test("+ with only one operand on the stack raises StackUnderflowException") {
    intercept[StackUnderflowException] {
      Forth().run("1 +")
    }
  }

  test("each arithmetic word raises StackUnderflowException when starved") {
    for word <- List("+", "-", "*", "/") do
      intercept[StackUnderflowException] {
        Forth().run(s"1 $word")
      }
  }
}
