import io.Source


sealed trait Square
final case object Floor extends Square
final case class Seat(occupied: Boolean) extends Square


object Grid {
  val directions = for (x <- List(-1, 0, 1); y <- List(-1, 0, 1) if x != 0 || y != 0) yield (x, y)

  private def parseLine(line: String) = {
    line.map{
      ch =>
        if (ch == '.')
          Floor
        else if (ch == 'L')
          Seat(false)
        else
          Seat(true)
    }.toVector
  }

  def apply(inputLines: Iterator[String]) =
    new Grid(inputLines.toVector.map(parseLine))
}


final class Grid(grid: Vector[Vector[Square]]) {
  val W = grid.size
  val H = grid(0).size

  private def countOccupied() =
    grid.flatten.count{
      case Seat(true) => true
      case _ => false
    }

  private def seek(i: Int, j: Int, di: Int, dj: Int): Boolean =
    if (i < 0 || j < 0 || i == W || j == H)
      false
    else
      grid(i)(j) match {
        case Seat(value) => value
        case Floor => seek(i + di, j + dj, di, dj)
      }

  private def updateSeat(isOccupied: Boolean, i: Int, j: Int) = {
    val numOccupied = Grid.directions.count{
      case (di, dj) => seek(i + di, j + dj, di, dj)
    }
    (!isOccupied && numOccupied == 0) || (isOccupied && numOccupied < 5)
  }

  private def updatePosition(i: Int, j: Int): Square = {
    val position = grid(i)(j)

    position match {
      case Floor => Floor
      case Seat(isOccupied) => Seat(updateSeat(isOccupied, i, j))
    }
  }

  def update(): Int = {
    val newGrid = (for (i <- 0 until W) yield {
      (for (j <- 0 until H) yield updatePosition(i, j)).toVector
    }).toVector

    if (newGrid == grid)
      countOccupied()
    else
      (new Grid(newGrid)).update()
  }
}

object Day11 extends App {
  val lines = Source.fromFile("inputs/input.txt").getLines()

  val grid = Grid(lines)
  val answer = grid.update()
  println(answer)
}