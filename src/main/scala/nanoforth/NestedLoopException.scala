package nanoforth

/** Thrown when a `DO ... LOOP` body contains another `DO`, i.e. a loop
  * nested inside a loop. The single-level parse would mis-read the
  * nested `LOOP` as the outer one; failing at parse time says what is
  * actually wrong.
  */
case class NestedLoopException() extends RuntimeException()
