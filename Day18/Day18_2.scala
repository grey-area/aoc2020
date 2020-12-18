import io.Source


object Evaluator {
  private val addPattern = """^(.+)\+(.+)$""".r
  private val mulPattern = """^(.+)\*(.+)$""".r
  private val numPattern = """^(\d+)$""".r
  private val parenPattern = """^(.*)\(([^\(\)]+)\)(.*)$""".r

  def apply(s1: String): Long = {
    val s = s1.replace(" ", "")
    s match {
      case parenPattern(pre, parenExpr, post) => Evaluator(f"$pre${Evaluator(parenExpr)}$post")
      case mulPattern(x, y) => Evaluator(x) * Evaluator(y)
      case addPattern(x, y) => Evaluator(x) + Evaluator(y)
      case numPattern(n) => n.toLong
    }
  }
}


object Day18 extends App {
  val lines = Source.fromFile("inputs/input.txt").getLines()

  val answer = lines.map(Evaluator(_)).sum
  println(answer)
}