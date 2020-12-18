object Evaluator {
  private val opPattern = """^(.+)([*+])(\d+)$""".r
  private val parenPattern = """^(.*)\(([^\(\)]+)\)(.*)$""".r

  private val + = (x: Long, y: Long) => x + y
  private val * = (x: Long, y: Long) => x * y
  private val ops = Map("+" -> +, "*" -> *)

  def apply(expr: String): Long = expr.replace(" ", "") match {
    case parenPattern(pre, parenExpr, post) => Evaluator(f"$pre${Evaluator(parenExpr)}$post")
    case opPattern(x, op, y) => ops(op)(Evaluator(x), Evaluator(y))
    case n => n.toLong
  }
}

object Day18 extends App {
  val lines = io.Source.fromFile("inputs/input.txt").getLines()
  println(lines.map(Evaluator(_)).sum)
}