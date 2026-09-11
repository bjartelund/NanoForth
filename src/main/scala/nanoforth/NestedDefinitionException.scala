package nanoforth

/** Thrown when a `:` appears inside a word's definition body (a
  * definition nested in a definition) or inside a `DO ... LOOP` body.
  * `runTokens` replays bodies from the top on every call, so a nested
  * `:` would silently (or confusingly) re-define a word on each
  * invocation — the error belongs at parse time, where it is actually
  * fixable.
  */
case class NestedDefinitionException() extends RuntimeException()
