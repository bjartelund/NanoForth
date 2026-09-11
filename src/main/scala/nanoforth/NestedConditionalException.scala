package nanoforth

/** Thrown when an `IF ... THEN` branch contains another conditional
  * structure (a nested `IF`/`ELSE`/`THEN`). The single-level parse
  * would then mis-read the nested markers as the outer ones; failing at
  * parse time says what is actually wrong.
  */
case class NestedConditionalException() extends RuntimeException()
