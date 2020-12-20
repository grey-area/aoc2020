import io.Source
import util.matching.Regex
import util.matching.Regex.Match

object Day20 extends App {
  def parseInput() = {
    val grid = Source.fromFile("inputs/part2_input.txt").getLines().toList
    val gridWidth = grid(0).size

    val monsterStr = Source.fromFile("inputs/sea_monster.txt").getLines().toList
    val monsterWidth = monsterStr(0).size
    val padding = "." * (gridWidth - monsterStr(0).size)
    val monsterPattern = monsterStr.mkString(padding).replace(' ', '.')
    val monsterIndices = monsterPattern.zipWithIndex.filter(_._1 == '#').map(_._2).toList
    (grid, monsterPattern.r, gridWidth, monsterWidth, monsterIndices)
  }

  type Grid = List[String]
  val flipTransforms = List(
    (g: Grid) => g: Grid,
    (g: Grid) => g.reverse: Grid,
    (g: Grid) => g.map(_.reverse): Grid
  )
  def rotateOnce(g: Grid) = g.transpose.map(_.mkString.reverse)
  def rotate(g: Grid, n: Int): Grid = if (n == 0) g else rotate(rotateOnce(g), n - 1)

  def removeMonster(grid: String, m: Match, monsterIndices: List[Int], gridWidth: Int, monsterWidth: Int) = {
    val start = m.start
    val rowStart = start % gridWidth
    val distanceFromRowEnd = gridWidth - rowStart

    if (distanceFromRowEnd < monsterWidth)
      grid
    else
      monsterIndices.foldLeft(grid){
        (grid, idx) => grid.updated(start + idx, '.')
      }
  }

  def removeAllMonsters(grid: String, pattern: Regex, monsterIndices: List[Int], gridWidth: Int, monsterWidth: Int): String = {
    val matches = pattern.findAllMatchIn(grid).toList

    if (matches.size == 0)
      grid
    else {
      val newGrid = matches.foldLeft(grid){
        (grid, m) => removeMonster(grid, m, monsterIndices, gridWidth, monsterWidth)
      }
      removeAllMonsters(newGrid, pattern, monsterIndices, gridWidth, monsterWidth)
    }
  }

  val (grid, monsterPattern, gridWidth, monsterWidth, monsterIndices) = parseInput()

  val transformedGrids = for {
    flipTransform <- flipTransforms
    rotation <- (0 until 4)
  } yield rotate(flipTransform(grid), rotation)

  val answer = transformedGrids.map{
    grid => removeAllMonsters(grid.mkString, monsterPattern, monsterIndices, gridWidth, monsterWidth).count(_ == '#')
  }.min
  println(answer)
}