object Evaluator {
  private val addPattern = """^(.+)\+(.+)$""".r
  private val mulPattern = """^(.+)\*(.+)$""".r
  private val numPattern = """^(\d+)$""".r
  private val parenPattern = """^(.*)\(([^\(\)]+)\)(.*)$""".r

  def apply(expr: String): Long =
    expr.replace(" ", "") match {
      case parenPattern(pre, parenExpr, post) => Evaluator(f"$pre${Evaluator(parenExpr)}$post")
      case mulPattern(x, y) => Evaluator(x) * Evaluator(y)
      case addPattern(x, y) => Evaluator(x) + Evaluator(y)
      case numPattern(n) => n.toLong
    }
}

object Day18 extends App {
  val lines = io.Source.fromFile("inputs/input.txt").getLines()
  println(lines.map(Evaluator(_)).sum)
}