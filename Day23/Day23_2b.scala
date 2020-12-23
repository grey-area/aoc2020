object Cups {
  def parseInput(fname: String) = {
    val startCups = io.Source.fromFile(fname).mkString.map(_.toString.toInt - 1) ++ ((9 until 1000000).toList)
    val orderMap = Map(startCups zip startCups.drop(1): _*) + (startCups.last -> startCups.head)
    val order = (0 to startCups.max).map(orderMap(_)).toVector
    Cups(startCups.head, order)
  }
}

final case class Cups(currentValue: Int, order: Vector[Int]) {
  private def computeAnswer() = {
    val x = order(0)
    (x.toLong + 1) * (order(x) + 1)
  }

  private def findInsertValue(removed: List[Int]) = {
    val values = Iterator.iterate(currentValue){
      x => (x - 1 + order.size) % order.size
    }.drop(1)
    values.find(v => ! (removed contains v)).get
  }

  private def removeAndInsert() = {
    val nextFour = Iterator.iterate(currentValue)(x => order(x)).drop(1).take(4).toList
    val removedKeys = nextFour.take(3)
    val insertValue = findInsertValue(removedKeys)
    order.updated(currentValue, nextFour.last).updated(insertValue, removedKeys.head).updated(removedKeys.last, order(insertValue))
  }

  def play(n: Int): Long =
    if (n == 0)
      computeAnswer()
    else {
      val newOrder = removeAndInsert()
      Cups(newOrder(currentValue), newOrder).play(n - 1)
    }
}


object Day23 extends App {
  val cups = Cups.parseInput("inputs/input.txt")
  val answer = cups.play(10000000)
  println(answer)
}