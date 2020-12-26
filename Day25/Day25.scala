final case class Transform(subject: Long) {
  def transform(value: Long) = (value * subject) % 20201227

  def transformLoop(loopSize: Int, value: Long = 1): Long =
    if (loopSize == 0)
      value
    else
      transformLoop(loopSize - 1, transform(value))

  def findLoopSize(target: Long, value: Long = 1, loopSize: Int = 0): Int =
    if (value == target)
      loopSize
    else
      findLoopSize(target, transform(value), loopSize + 1)
}

object Day25 extends App {
  def parseInput(fname: String) = {
    val lines = io.Source.fromFile(fname).getLines().toList
    (lines(0).toLong, lines(1).toLong)
  }

  val (cardPublicKey, doorPublicKey) = parseInput("inputs/input.txt")

  val doorLoopSize = Transform(7).findLoopSize(doorPublicKey)
  val answer = Transform(cardPublicKey).transformLoop(doorLoopSize)
  println(answer)
}