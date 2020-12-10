import io.Source


object Day10 extends App {
  def parseInput(fname: String) = {
    val input = Source.fromFile(fname).getLines().map(_.toInt).toVector.sorted
    0 +: input :+ (3 + input.max)
  }

  val joltages = parseInput("inputs/input.txt")
  val differences = (joltages zip joltages.tail) map (t => t._2 - t._1)

  val counts = differences.groupBy(identity).mapValues(_.size)

  val answer = counts.getOrElse(1, 0) * counts.getOrElse(3, 0)
  println(answer)
}