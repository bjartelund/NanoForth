package nanoforth

/** Stage 3: stack-shuffling words `DUP DROP SWAP OVER`.
  *
  * These don't do arithmetic — they only rearrange what's already on the
  * stack — but each still has an arity and can underflow just like `+`
  * or `-` did in stage 2, so they should reuse that same
  * `StackUnderflowException` machinery rather than inventing a new one.
  *
  * Forth words are conventionally written in upper case; nano-Forth
  * treats case as significant, so "dup" is just an unknown word, not an
  * alias for "DUP".
  */
class Stage3StackWordsSpec extends munit.FunSuite {

  test("DUP duplicates the top of the stack") {
    assertEquals(Forth().run("5 DUP").stack, Vector(5L, 5L))
  }

  test("DUP on an empty stack raises StackUnderflowException") {
    intercept[StackUnderflowException] {
      Forth().run("DUP")
    }
  }

  test("DROP removes the top of the stack") {
    assertEquals(Forth().run("5 6 DROP").stack, Vector(5L))
  }

  test("DROP on an empty stack raises StackUnderflowException") {
    intercept[StackUnderflowException] {
      Forth().run("DROP")
    }
  }

  test("SWAP exchanges the top two values") {
    assertEquals(Forth().run("1 2 SWAP").stack, Vector(2L, 1L))
  }

  test("SWAP with fewer than two values raises StackUnderflowException") {
    intercept[StackUnderflowException] {
      Forth().run("1 SWAP")
    }
  }

  test("OVER copies the second-from-top value onto the top") {
    assertEquals(Forth().run("1 2 OVER").stack, Vector(1L, 2L, 1L))
  }

  test("OVER with fewer than two values raises StackUnderflowException") {
    intercept[StackUnderflowException] {
      Forth().run("1 OVER")
    }
  }

  test("stack words compose with each other and with arithmetic") {
    // push 1 2 3 -> [1,2,3]; SWAP top two -> [1,3,2]; DROP top -> [1,3]
    assertEquals(Forth().run("1 2 3 SWAP DROP").stack, Vector(1L, 3L))

    // DUP the top, then add it to itself: "5 DUP +" => 5 + 5
    assertEquals(Forth().run("5 DUP +").stack, Vector(10L))

    // OVER then + : "3 4 OVER +" => stack [3,4,3] -> add top two -> [3,7]
    assertEquals(Forth().run("3 4 OVER +").stack, Vector(3L, 7L))
  }

  test("lower-case is not recognized as a stack word") {
    intercept[NoSuchElementException] {
      Forth().run("5 dup")
    }
  }
}
