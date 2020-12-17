import io.Source


final case class Position(x: Int, y: Int, z: Int) {
  def +(p2: Position) = Position(x + p2.x, y + p2.y, z + p2.z)
}


object Grid {
  def range(min: Int, max: Int) = {
    val rs = min to max
    for (x <- rs; y <- rs; z <- rs) yield Position(x, y, z)
  }

  val neighbourhood = range(-1, 1)

  private def parseLine(line: String, i: Int) =
    line.zipWithIndex.map{
      case (ch, j) => Position(i, j, 0) -> (ch == '#')
    }

  def parse(inputLines: List[String]) = {
    val states = Map(inputLines.zipWithIndex.flatMap{
      case (line, i) => parseLine(line, i)
    }: _*)

    new Grid(states, -1, inputLines.size)
  }
}


final class Grid(private val states: Map[Position, Boolean], private val minExtent: Int, private val maxExtent: Int) {
  def countActive = states.values.count(identity)

  private def neighbourCount(pos: Position) =
    Grid.neighbourhood.count{
      offset => states.getOrElse(pos + offset, false)
    }

  private def cellState(pos: Position) = {
    val count = neighbourCount(pos)
    val prevState = states.getOrElse(pos, false)

    (prevState && (3 to 4 contains count)) ||
      (!prevState && count == 3)
  }

  def update(steps: Int): Grid = {
    val newStates = Map(Grid.range(minExtent, maxExtent).map(pos => pos -> cellState(pos)): _*)
    val newGrid = new Grid(newStates, minExtent - 1, maxExtent + 1)

    if (steps == 1)
      newGrid
    else
      newGrid.update(steps - 1)
  }
}


object Day17 extends App {
  val lines = Source.fromFile("inputs/input.txt").getLines().toList

  val grid = Grid.parse(lines)
  val answer = grid.update(steps=6).countActive

  println(answer)
}