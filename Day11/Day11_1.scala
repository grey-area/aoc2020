import io.Source


sealed trait Square
final case object Floor extends Square
final case class Seat(occupied: Boolean) extends Square


object GridHelper {
  def pad[A](input: Vector[A], padValue: A): Vector[A] =
    padValue +: input :+ padValue

  def countOccupied(subgrid: Vector[Square]) =
    subgrid.count{
      case Seat(true) => true
      case _ => false
    }

  private def memoize2[K1, K2, V](f: (K1, K2) => V): (K1, K2) => V = {
    val cache = collection.mutable.Map.empty[Tuple2[K1, K2], V]
    (k1, k2) => cache.getOrElseUpdate((k1, k2), f(k1, k2))
  }

  private def _computeOccupation(isOccupied: Boolean, subgrid: Vector[Square]) = {
    val numOccupied = countOccupied(subgrid)
    val newOccupation = (!isOccupied && numOccupied == 0) || (isOccupied && numOccupied < 5)
    newOccupation
  }
  val computeOccupation = memoize2(_computeOccupation)

  private def parseLine(line: String) = {
    val gridLine = line.map{
      ch =>
        if (ch == '.')
          Floor
        else if (ch == 'L')
          Seat(false)
        else
          Seat(true)
    }.toVector
    pad(gridLine, Floor)
  }

  def parse(inputLines: Iterator[String]) = {
    val grid = inputLines.toVector.map(parseLine)
    val W = grid(0).size
    val blankLine = (for (_ <- 1 to W) yield Floor).toVector
    Grid(pad(grid, blankLine))
  }
}


final case class Grid(grid: Vector[Vector[Square]]) {
  private def updatePosition(i: Int, j: Int): Square = {
    val position = grid(i)(j)

    position match {
      case Floor => Floor
      case Seat(isOccupied) => {
        val subgrid = grid.slice(i - 1, i + 2).flatMap(_.slice(j - 1, j + 2))
        val newOccupation = GridHelper.computeOccupation(isOccupied, subgrid)
        Seat(newOccupation)
      }
    }
  }

  def update(): Int = {
    val W = grid.size - 2
    val H = grid(0).size - 2

    val newGrid = (for (i <- 0 to W + 1) yield {
      (for (j <- 0 to H + 1) yield {
        if (i == 0 || i == W + 1 || j == 0 || j == H + 1)
          Floor
        else
          updatePosition(i, j)
      }).toVector
    }).toVector

    if (newGrid == grid)
      GridHelper.countOccupied(grid.flatten)
    else
      Grid(newGrid).update()
  }
}


object Day11 extends App {
  val lines = Source.fromFile("inputs/input.txt").getLines()

  val grid = GridHelper.parse(lines)
  val answer = grid.update()
  println(answer)
}