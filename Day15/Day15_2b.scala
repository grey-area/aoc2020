// Trying a mutable map inside an imperative play method to compare efficiency

object Elves {
  def apply(limit: Int)(startingNumbers: Long*) = {
    val initialHistory = Map(startingNumbers.zipWithIndex: _*)
    new Elves(limit, initialHistory)
  }
}

final class Elves(private val limit: Int, private val initialHistory: Map[Long, Int]) {
  def play(previousStart: Int, i: Int = initialHistory.size): Int = {
    var previous = previousStart
    val history = collection.mutable.LongMap(initialHistory.toSeq: _*)

    for (i <- initialHistory.size until limit - 1) {
      val speak = if (history contains previous) i - history(previous) else 0
      history.update(previous, i)
      previous = speak
    }
    previous
  }
}


object Day15 extends App {
  val elves = Elves(limit=30000000)(1, 2, 16, 19, 18)
  val answer = elves.play(0)
  println(answer)
}