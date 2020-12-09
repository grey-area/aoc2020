import io.Source

final case class Edge(toName: String, number: Int)

object Graph {
  def fromBagPattern = raw"^(\w+\s\w+)\sbag".r
  def toBagPattern = raw"(?<=\s)(\d+)\s(\w+\s\w+)\sbag".r

  def parseLine(line: String) = {
    val fromColour = fromBagPattern.findFirstMatchIn(line).map(_.group(1))
    val toColours = toBagPattern.findAllMatchIn(line).map{
      m => Edge(m.group(2), m.group(1).toInt)
    }.toList

    fromColour match {
      case Some(v) => Some(v -> toColours)
      case None => None
    }
  }

  def apply(lines: Iterator[String]) = {
    val graphMap = Map(lines.flatMap(parseLine).toList: _*)
    new Graph(graphMap)
  }
}

final class Graph(private val graphMap: Map[String, List[Edge]]) {
  // Note: inefficient, repeated computation
  def countSubBags(key: String): Int = {
    val values = graphMap(key)
    if (values.size == 0)
      0
    else
      values.map(edge => edge.number * (1 + countSubBags(edge.toName))).sum
  }
}

object Day07 extends App {
  val lines = Source.fromFile("inputs/input.txt").getLines()
  val graph = Graph(lines)

  println(graph.countSubBags("shiny gold"))
}