object Tile{
  val HeaderPattern = """Tile (\d+):""".r
  val W = 10

  val flipTransforms = List(
    (t: Tile) => t,
    (t: Tile) => Tile(t.id, t.bottom, t.right.reverse, t.top, t.left.reverse),
    (t: Tile) => Tile(t.id, t.top.reverse, t.left, t.bottom.reverse, t.right)
  )

  def parse(s: String) = {
    val (header :: tileLines) = s.split('\n').toList
    val id = (header match {case HeaderPattern(id) => id}).toInt
    val top = tileLines(0)
    val right = tileLines.map(_(W - 1)).mkString
    val bottom = tileLines(W - 1)
    val left = tileLines.map(_(0)).mkString
    id -> Tile(id, top, right, bottom, left)
  }
}

final case class Tile(id: Int, top: String, right: String, bottom: String, left: String) {
  private def rotateOnce = Tile(id, left.reverse, top, right.reverse, bottom)
  def rotate(n: Int): Tile = if (n == 0) this else this.rotateOnce.rotate(n - 1)
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
}


object Day20 extends App {
  val tiles = Map(io.Source.fromFile("inputs/input.txt").mkString.split("\n\n").toList.map(Tile.parse): _*)

  val assignment = Arrangement.allValidAssignments(0, tiles.keys.toSet, tiles).head
  println(Arrangement.getCornerIDs(assignment).product)
}