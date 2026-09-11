package nanoforth

@main def run(): Unit =
  Iterator.continually(scala.io.StdIn.readLine())
    .takeWhile(_ != null)
    .foldLeft(Forth()) { (forth, line) =>
      val (next, shown) = Repl.step(forth, line)
      println(shown)
      next
    }