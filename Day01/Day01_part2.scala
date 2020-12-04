import scala.io.Source

object Day01 extends App {
  def parse_input(): List[Int] =
    (for (x <- Source.fromFile("input.txt").getLines()) yield x.toInt).toList

  def prefilter(xs: List[Int]) =
    xs.filter(_ <= 2020 - 2 * xs.min)

  val xs = prefilter(parse_input())
  val pairs = xs.combinations(2).toList
  val xs_set = xs.toSet

  val entry = pairs.find{
    l => xs_set.contains(2020 - l(0) - l(1))
  }

  val answer = entry.map{
    l => l(0) * l(1) * (2020 - l(0) - l(1))
  }

  answer match {
    case None => println("No answer found")
    case Some(v) => println(v)
  }
}