object Cups {
  def parseInput(fname: String) = {
    val startCups = io.Source.fromFile(fname).mkString.map(_.toString.toInt) ++ ((10 to 1000000).toList)
    val order = Map(startCups zip startCups.drop(1): _*) + (startCups.last -> startCups.head)
    Cups(startCups.head, order)
  }
}

final case class Cups(currentValue: Int, order: Map[Int, Int]) {
  private def computeAnswer() = {
    val x = order(1)
    x.toLong * order(x)
  }

  private def findInsertValue(removed: List[Int]) = {
    val values = Iterator.iterate(currentValue){
      x => (x - 2 + order.size) % order.size + 1
    }.drop(1)
    values.find(v => ! (removed contains v)).get
  }

  private def removeAndInsert() = {
    val nextFour = Iterator.iterate(currentValue)(x => order(x)).drop(1).take(4).toList
    val removedKeys = nextFour.take(3)
    val insertValue = findInsertValue(removedKeys)
    order ++ Map(currentValue -> nextFour.last, insertValue -> removedKeys.head, removedKeys.last -> order(insertValue))
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