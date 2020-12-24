object HexGrid {
  private val pattern = "[ns]?[we]".r
  private def parseLine(s: String) = pattern.findAllIn(s).toList

  private def computePosition(line: List[String], pos: (Int, Int)): (Int, Int) = line match {
    case Nil => pos
    case dir :: tail => {
      val (x, y) = pos
      val newPos = dir match {
        case "nw" => (x - 1, y + 1)
        case "ne" => (x, y + 1)
        case "e"  => (x + 1, y)
        case "se" => (x + 1, y - 1)
        case "sw" => (x, y - 1)
        case "w"  => (x - 1, y)
      }
      computePosition(tail, newPos)
    }
  }

  def processInput(fname: String) = {
    val lines = io.Source.fromFile(fname).getLines().map(parseLine).toList
    val grid = HexGrid()
    grid.update(lines)
  }
}

final case class HexGrid(grid: Set[(Int, Int)] = Set.empty) {
  def countBlack = grid.size

  private def update(lines: List[List[String]], pos: (Int, Int) = (0, 0)): HexGrid = lines match {
    case Nil => this
    case line :: tail => {
      val newPos = HexGrid.computePosition(line, pos)
      val newGrid = if (grid contains newPos) grid - newPos else grid + newPos
      HexGrid(newGrid).update(tail)
    }
  }
}


object Day24 extends App {
  val finalState = HexGrid.processInput("inputs/input.txt")
  val answer = finalState.countBlack
  println(answer)
}