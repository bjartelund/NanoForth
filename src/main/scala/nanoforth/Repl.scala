package nanoforth

/** The pure, testable heart of a REPL: given the interpreter's current
  * state and one line of input, produce the next state and the text
  * that should be shown to the user.
  *
  * This is deliberately separate from anything that touches stdin or
  * stdout. `Main` (untested, by design — there's nothing meaningful to
  * assert against a live terminal) is just a thin loop: read a line,
  * call `step`, print what comes back, repeat.
  *
  * On success, only the output *this line* produced is shown, followed
  * by Forth's traditional "ok" — e.g. running "3 4 + ." on a fresh
  * interpreter shows "7 ok", and a line that prints nothing shows just
  * "ok". On failure, the exception is reported and the *previous*
  * state is returned unchanged, so one bad line doesn't corrupt the
  * session.
  */
object Repl {
  def step(forth: Forth, line: String): (Forth, String) =
    try 
      val result = forth.run(line)
      val newOutput  = result.output.stripPrefix(forth.output)
      (result, newOutput+  "ok")
    catch
      case e: Exception => (forth,s"Error ${e.getClass.getSimpleName}")  
}
