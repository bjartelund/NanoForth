package nanoforth

import java.util.NoSuchElementException

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

  /** Evaluate a single whitespace-delimited token against the current
    * state, returning the new state.
    */
  def eval(word: String): Forth =
    word match
      case emptyString if emptyString.isBlank => this

      case number if number.toLongOption.isDefined => copy(stack = stack :+ number.toLong)

      case unknownElement => throw NoSuchElementException(unknownElement)

  /** Evaluate a whole line of input, left to right. */
  def run(input: String): Forth =
    input.split(' ').foldLeft(this)((forth,word) => forth.eval(word))
}
