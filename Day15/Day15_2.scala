object Elves {
  def apply(limit: Int)(startingNumbers: Int*) = {
    val initialHistory = Map(startingNumbers.zipWithIndex: _*)
    new Elves(limit, initialHistory)
  }
}

final class Elves(private val limit: Int, private val initialHistory: Map[Int, Int]) {
  def play(previous: Int, i: Int = initialHistory.size, history: Map[Int, Int] = initialHistory): Int =
    if (i + 1 == limit) previous else {
      val speak = if (history contains previous) i - history(previous) else 0
      play(speak, i + 1, history + (previous -> i))
    }
}


object Day15 extends App {
  val elves = Elves(limit=30000000)(1, 2, 16, 19, 18)
  val answer = elves.play(0)
  println(answer)
}