import java.io.PrintWriter


object Tile{
  val HeaderPattern = """Tile (\d+):""".r
  val W = 10

  val flipTransforms = List(
    (t: Tile) => t,
    (t: Tile) => Tile(t.id, t.lines.map(_.reverse)),
    (t: Tile) => Tile(t.id, t.lines.reverse)
  )

  def parse(s: String) = {
    val (header :: tileLines) = s.split('\n').toList
    val id = (header match {case HeaderPattern(id) => id}).toInt
    id -> Tile(id, tileLines)
  }
}

final case class Tile(id: Int, lines: List[String]) {
  private def rotateOnce = Tile(id, lines.transpose.map(_.mkString.reverse))
  def rotate(n: Int): Tile = if (n == 0) this else this.rotateOnce.rotate(n - 1)
  def top = lines(0)
  def bottom = lines(Tile.W - 1)
  def left = lines.map(_(0)).mkString
  def right = lines.map(_(Tile.W - 1)).mkString
  private def lineWithoutBorder(line: String) = line.slice(1, Tile.W - 1)
  def withoutBorder = lines.slice(1, Tile.W - 1).map(lineWithoutBorder(_))
}


object Arrangement {
  private val N = 12

  def getCornerIDs(solution: Map[(Int, Int), Tile]) =
    for (i <- List(0, N - 1); j <- List(0, N - 1)) yield solution(i, j).id.toLong

  def allValidAssignments(pos: Int, remaining: Set[Int], tiles: Map[Int, Tile], solution: Map[(Int, Int), Tile] = Map.empty): LazyList[Map[(Int, Int), Tile]] =
    if (pos == N * N)
      LazyList(solution)
    else
    {
      val potentialAssignments = for {
        i <- remaining.to(LazyList)
        flipTransform <- Tile.flipTransforms
        rotateTransform <- (0 until 4).map(n => (t: Tile) => t.rotate(n))
      } yield rotateTransform(flipTransform(tiles(i)))

      val (i, j) = (pos % N, pos / N)

      val validAssignments = potentialAssignments.filter{
        tile => (i == 0 || tile.left == solution((i - 1, j)).right) &&
          (j == 0 || tile.top == solution((i, j - 1)).bottom)
      }

      validAssignments.flatMap{
        tile => allValidAssignments(pos + 1, remaining - tile.id, tiles, solution + ((i, j) -> tile))
      }
    }

  def produceMap(solution: Map[(Int, Int), Tile]) =
    (for (j <- 0 until N; j2 <- 0 until Tile.W - 2) yield {
      (for (i <- 0 until N) yield solution((i, j)).withoutBorder(j2)).mkString
    }).mkString("\n")
}


object Day20 extends App {
  val tiles = Map(io.Source.fromFile("inputs/input.txt").mkString.split("\n\n").toList.map(Tile.parse): _*)

  val assignment = Arrangement.allValidAssignments(0, tiles.keys.toSet, tiles).head
  val answer = Arrangement.getCornerIDs(assignment).product
  println(answer)

  new PrintWriter("inputs/part2_input.txt") { write(Arrangement.produceMap(assignment)); close }
}