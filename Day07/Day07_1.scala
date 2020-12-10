import io.Source

object Graph {
  private def fromBagPattern = raw"^(\w+\s\w+)\sbag".r
  private def toBagPattern = raw"(?<=\s)(\d+)\s(\w+\s\w+)\sbag".r

  private def parseLine(line: String) = {
    val fromColour = fromBagPattern.findFirstMatchIn(line).map(_.group(1))
    val toColours = toBagPattern.findAllMatchIn(line).map{_.group(2)}.toSet

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

final class Graph(private val graphMap: Map[String, Set[String]]) {
  private def memoize2[K1, K2, V](f: (K1, K2) => V): (K1, K2) => V = {
    val cache = collection.mutable.Map.empty[Tuple2[K1, K2], V]
    (k1, k2) => cache.getOrElseUpdate((k1, k2), f(k1, k2))
  }

  private def _canReach(key: String, target: String): Boolean = {
    val values = graphMap(key)
    values.size > 0 && (values.contains(target) ||
      values.map(canReach(_, target)).reduce(_ || _))
  }

  private val canReach = memoize2(_canReach)

  def numberReaching(target: String) =
    graphMap.keys.toList.map(x => if (canReach(x, target)) 1 else 0).sum
}

object Day07 extends App {
  val lines = Source.fromFile("inputs/input.txt").getLines()
  val graph = Graph(lines)

  println(graph.numberReaching("shiny gold"))
}