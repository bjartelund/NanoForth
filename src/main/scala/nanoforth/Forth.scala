package nanoforth

import java.util.NoSuchElementException
import scala.annotation.tailrec
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

object Forth:
  private val builtins: Map[String, Primitive] = Map(
    "+" -> (_.add),
    "-" -> (_.sub),
    "*" -> (_.mul),
    "/" -> (_.div),
    "=" -> (_.equal),
    "<" -> (_.lessThan),
    ">" -> (_.greaterThan),
    "." -> (_.dot),
    ".S" -> (_.dotS),
    "I" -> (_.i),
    "AND" -> (_.and),
    "OR" -> (_.or),
    "SWAP" -> (_.swap),
    "OVER" -> (_.over),
    "DUP" -> (_.dup),
    "DROP" -> (_.drop)
  )

case class Forth(stack: Vector[Long] = Vector.empty, output: String = "", dictionary: Map[String,Primitive] = Forth.builtins, loopIndices: Vector[Long] = Vector.empty) {

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


  // The underflow guard lives in `pop` itself, so `pop2` can simply call
  // it — no separate size check on the caller side. `pop` reaches for
  // `stack.last` unconditionally, so on a starved stack it would otherwise
  // blow up with a raw `NoSuchElementException` (empty stack) after
  // having already popped the stack's last remaining element — a raw,
  // implementation-detail exception that leaks how the stack happens to
  // be represented. The guard turns that into a `StackUnderflowException`
  // instead, naming the real Forth-level problem (not enough operands)
  // rather than an accidental one (calling `.last` on an empty Vector).

  private def dot : Forth =
    val (a,forth) = pop
    copy(stack = forth.stack,output = output + a.toString + " ")

  private def dotS : Forth =
    if stack.isEmpty then this
    else copy(output = output + stack.mkString(" ") + " ")

  private def i : Forth =
    push(loopIndices.last)

  private def equal : Forth =
    val (b, a, forth) = pop2
    if a == b then
      forth.push(-1)
    else
      forth.push(0)

  private def lessThan : Forth =
    val (b, a, forth) = pop2
    if a < b then
      forth.push(-1)
    else
      forth.push(0)

  private def greaterThan: Forth =
    val (b, a, forth) = pop2
    if a > b then
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


  def run(input: String): Forth =
    runTokens(input.split("\\s+").toList)

  private def replay(body: List[String]): Primitive =
    state => state.runTokens(body)

  private def loop(index: Long, limit: Long, body: List[String]) : Forth =
    if index >= limit then this
    else
      val withIndex = copy(loopIndices = loopIndices :+ index)
      val afterBody = withIndex.runTokens(body)
      val cleaned = afterBody.copy(loopIndices = afterBody.loopIndices.dropRight(1))
      cleaned.loop(index+1,limit, body)

  @tailrec
  private def runTokens(tokens: List[String]): Forth =
    tokens match
      case List() =>
        this

      case ":" :: name :: rest =>
        if !rest.contains(";") then throw UnterminatedDefinitionException()

        val (body, remaining) =
          rest.span(_ != ";")

        // A `:` inside the body would be replayed on every call,
        // re-defining a word over and over. Reject at parse time.
        if body.contains(":") then throw NestedDefinitionException()

        val definition: Primitive =replay(body)

        copy(
          dictionary =
            dictionary + (name -> definition)
        ).runTokens(remaining.tail)


      case "IF" :: rest =>
        if !rest.contains("THEN") then throw UnterminatedConditionalException()
        val (flag, afterPop) = pop

        val (trueBranch,afterTrueBranch) = rest.span(w=> w!= "ELSE" && w != "THEN")
        val (falseBranch, tokensAfterThen) = afterTrueBranch match
          case "THEN" :: after => (Nil, after)
          case "ELSE" :: afterElse =>
            afterElse.span(_ != "THEN") match
              case (fb, "THEN" :: after) => (fb, after)
              case _ => throw UnterminatedConditionalException()
          case _ => throw UnterminatedConditionalException()

        // A nested IF/ELSE in either branch would be misread as the
        // outer markers by the single-level parse above. Reject it.
        if (trueBranch ++ falseBranch).exists(w => w == "IF" || w == "ELSE")
          then throw NestedConditionalException()

        val chosenBranch = if flag != 0 then trueBranch else falseBranch

        afterPop.runTokens(chosenBranch++tokensAfterThen)

      case "DO" :: rest =>
        val (index,limit,afterPop) = pop2

        val (body,tokensAfterLoop) = rest.span(_ != "LOOP") match
          case (b,"LOOP" :: after ) => (b,after)
          case _ => throw UnterminatedLoopException()

        // A nested DO or `:` in the body would be misread (or replayed
        // on every iteration) by the single-level parse above. Reject it.
        if body.contains("DO") then throw NestedLoopException()
        if body.contains(":") then throw NestedDefinitionException()

        afterPop.loop(index, limit, body).runTokens(tokensAfterLoop)

      case word :: rest =>
        eval(word).runTokens(rest)
}
