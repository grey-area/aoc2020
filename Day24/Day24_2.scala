import java.io.{ObjectInputStream, FileInputStream}


final case class Position(x: Int, y: Int) {
  def +(p2: Position) = Position(x + p2.x, y + p2.y)
}

object Grid {
  def range(min: Int, max: Int) = {
    val rs = min to max
    for (x <- rs; y <- rs) yield Position(x, y)
  }

  val neighbourhood = List(
    Position(1, 0), Position(1, -1), Position(0, -1),
    Position(-1, 0), Position(-1, 1), Position(0, 1)
  )

  def load(fname: String) = {
    val ois = new ObjectInputStream(new FileInputStream(fname))
    val blackHexes = ois.readObject.asInstanceOf[Set[(Int, Int)]].toList
    ois.close

    val blankGrid = Map((for (x <- -20 to 20; y <- -20 to 20) yield (Position(x, y) -> false)): _*)
    Grid(blankGrid ++ Map(blackHexes.map(p => Position(p._1, p._2) -> true): _*), -20, 20)
  }
}

case class Grid(states: Map[Position, Boolean], minExtent: Int, maxExtent: Int) {
  private def countBlack = states.values.count(identity)

  private def neighbourCount(pos: Position) =
    Grid.neighbourhood.count{
      offset => states.getOrElse(pos + offset, false)
    }

  private def cellState(pos: Position) = {
    val count = neighbourCount(pos)
    val prevState = states.getOrElse(pos, false)

    (prevState && (count == 1 || count == 2)) ||
      (!prevState && count == 2)
  }

  def update(steps: Int): Int =
    if (steps == 0)
      countBlack
    else {
      val newStates = Map(Grid.range(minExtent, maxExtent).map(pos => pos -> cellState(pos)): _*)
      Grid(newStates, minExtent - 1, maxExtent + 1).update(steps - 1)
    }
}


object Day24 extends App {
  val grid = Grid.load("inputs/part2_generated_input")
  val answer = grid.update(100)
  println(answer)
}