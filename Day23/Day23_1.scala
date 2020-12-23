object Cups {
  private def rotate(values: Vector[Int], n: Int) =
    values.drop(n) ++ values.take(n)

  def parseInput(fname: String) =
    Cups(io.Source.fromFile(fname).mkString.map(_.toString.toInt).toVector)
}

final case class Cups(values: Vector[Int]) {
  private def findNextValue(removed: Vector[Int], currentValue: Int) = {
    val values = Iterator.iterate(currentValue){
      x => (x - 2 + 9) % 9 + 1
    }.drop(1)
    values.find(v => ! (removed contains v)).get
  }

  private def removeAndInsert() = {
    val removed = values.slice(1, 4)
    val remaining = values.head +: values.slice(4, values.size)
    val destinationIndex = remaining.indexOf(findNextValue(removed, values(0))) + 1
    remaining.take(destinationIndex) ++ removed ++ remaining.drop(destinationIndex)
  }

  private def computeAnswer() = {
    val idx = values.indexOf(1)
    val rotated = Cups.rotate(values, idx)
    rotated.drop(1).mkString
  }

  def play(n: Int): String =
    if (n == 0)
      computeAnswer()
    else {
      val updatedValues = Cups.rotate(removeAndInsert(), 1)
      Cups(updatedValues).play(n - 1)
    }
}


object Day23 extends App {
  val cups = Cups.parseInput("inputs/input.txt")
  val answer = cups.play(100)
  println(answer)
}