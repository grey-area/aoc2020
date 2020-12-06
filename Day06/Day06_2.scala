import io.Source

object Day06 extends App {
  def parseInput(fname: String) =
    Source.fromFile(fname).mkString.split("\n\n").map(_.split("\n").toList).toList

  def positiveAnswersInGroup(group: List[String]) = {
    val group_sets = group.map(_.toSet)
    group_sets.reduceLeft(_ intersect _).size
  }

  val groups = parseInput("input.txt")

  val positiveAnswers = groups.map(positiveAnswersInGroup).sum
  println(positiveAnswers)
}