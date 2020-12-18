object Evaluator {
  private val opPattern = """^(.+)([*+])(\d+)$""".r
  private val numPattern = """^(\d+)$""".r
  private val parenPattern = """^(.*)\(([^\(\)]+)\)(.*)$""".r

  private val + = (x: Long, y: Long) => x + y
  private val * = (x: Long, y: Long) => x * y
  private val ops = Map("+" -> +, "*" -> *)

  def apply(s1: String): Long = {
    val s = s1.replace(" ", "")
    s match {
      case parenPattern(pre, parenExpr, post) => Evaluator(f"$pre${Evaluator(parenExpr)}$post")
      case opPattern(x, op, y) => ops(op)(Evaluator(x), Evaluator(y))
      case numPattern(n) => n.toLong
    }
  }
}

object Day18 extends App {
  val lines = io.Source.fromFile("inputs/input.txt").getLines()
  println(lines.map(Evaluator(_)).sum)
}