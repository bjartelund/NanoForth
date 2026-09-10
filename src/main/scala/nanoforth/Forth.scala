package nanoforth

import java.util.NoSuchElementException
import scala.collection.immutable.Map

type Primitive = Forth => Forth

/** The state of a nano-Forth interpreter: just a data stack, for now.
  *
  * Forth is stack-based: numbers get pushed, words consume and produce
  * values on that same stack. We model the interpreter as an immutable
  * value — evaluating input never mutates anything, it returns a new
  * `Forth` with an updated `stack`. This mirrors how Forth itself works
  * (one shared stack, threaded through every word) while staying in a
  * style that's easy to reason about and test.
  *
  * `stack` is ordered from bottom to top: the last element is the top
  * of the stack (the most recently pushed value).
  */

case class Forth(stack: Vector[Long] = Vector.empty) {

  private val dictionary: Map[String,Primitive] = Map(
    "+" -> (_.add),
    "-" -> (_.sub),
    "*" -> (_.mul),
    "/" -> (_.div),
    "=" -> (_.equal),
    ">" -> (_.lessThan),
    "<" -> (_.greaterThan),
    "AND" -> (_.and),
    "OR" -> (_.or),
    "SWAP" -> (_.swap),
    "OVER" -> (_.over),
    "DUP" -> (_.dup),
    "DROP" -> (_.drop)
  )

  /** Evaluate a single whitespace-delimited token against the current
    * state, returning the new state.
    */

  private def pop: (Long, Forth) =
    if stack.size < 1 then throw StackUnderflowException()
    (stack.last, copy(stack = stack.dropRight(1)))

  /** Pop the top two values for a binary word, e.g. "a b op".
    *
    * The first value popped (`b`) is the top of the stack — the operand
    * that was pushed *last* — and the second (`a`) is the one below it.
    * Returning them as `(b, a, forth)`, in pop order rather than push
    * order, is what forces every call site to stop and ask "which one
    * was on top?" instead of assuming left-to-right = source order.
    * That's deliberate: `sub`/`div` need `a - b` / `a / b`, and a
    * `(a, b, forth)` tuple would make it dangerously easy to get that
    * backwards without noticing, since `a op b` reads fine either way
    * until you check the actual arithmetic.
    */
  private def pop2: (Long,Long,Forth) =
    val (b, iForth) = pop
    val (a, jForth) = iForth.pop
    (b,a,jForth)

  private def push(value: Long) : Forth =
    copy(stack = stack :+ value)


  // Each binary word checks `stack.size < 2` before calling `pop2`, rather
  // than letting `pop2` fail on its own. `pop` reaches for `stack.last`
  // unconditionally, so on a starved stack it would blow up with
  // `NoSuchElementException` (empty stack) or run once, then have the
  // *second* pop fail the same way on a one-element stack — a raw,
  // implementation-detail exception that leaks how the stack happens to
  // be represented. The guard turns that into a `StackUnderflowException`
  // up front, naming the real Forth-level problem (not enough operands)
  // instead of an accidental one (calling `.last` on an empty Vector).

  private def equal : Forth =
    val (b, a, forth) = pop2
    if a == b then
      forth.push(-1)
    else
      forth.push(0)

  private def lessThan : Forth =
    val (b, a, forth) = pop2
    if a > b then
      forth.push(-1)
    else
      forth.push(0)

  private def greaterThan: Forth =
    val (b, a, forth) = pop2
    if a < b then
      forth.push(-1)
    else
      forth.push(0)

  private def and: Forth =
    val (b, a, forth) = pop2
    if a != 0 && b != 0 then
      forth.push(-1)
    else
      forth.push(0)

  private def or: Forth =
    val (b, a, forth) = pop2
    if a != 0 || b != 0 then
      forth.push(-1)
    else
      forth.push(0)

  private def add : Forth =
    val (b, a, forth) = pop2
    forth.push(b+a)

  private def sub : Forth =
    val (b, a, forth) = pop2
    forth.push(a - b)

  private def mul: Forth =
    val (b, a, forth) = pop2
    forth.push(a * b)

  private def div: Forth =
    val (b, a, forth) = pop2
    forth.push(a / b)

  private def dup: Forth =
    val (a,_) = pop
    copy(stack = stack :+ a)

  private def drop: Forth =
    val (_,newForth) = pop
    newForth

  private def swap: Forth =
    val (b,a, forth) = pop2
    forth.push(b).push(a)

  private def over: Forth =
    val (b, a, forth) = pop2
    forth.push(a).push(b).push(a)

  def eval(word: String): Forth =
    word match
      case emptyString if emptyString.isBlank => this

      case number if number.toLongOption.isDefined => copy(stack = stack :+ number.toLong)

      case _ => dictionary.getOrElse(word, throw NoSuchElementException(word))(this)


  /** Evaluate a whole line of input, left to right. */
  def run(input: String): Forth =
    input.split(' ').foldLeft(this)((forth,word) => forth.eval(word))
}
