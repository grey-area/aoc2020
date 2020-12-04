import scala.io.Source

object Day01 extends App {
  def parse_input(): Set[Int] =
    (for (x <- Source.fromFile("input.txt").getLines()) yield x.toInt).toSet

  val xs = parse_input()

  val entry: Option[Int] = xs.find{
    x => xs.contains(2020 - x)
  }

  val answer = entry.map(x => x * (2020 - x))

  answer match {
    case None => println("No answer found")
    case Some(v) => println(v)
  }
}