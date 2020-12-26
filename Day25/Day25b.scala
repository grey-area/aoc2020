final case class Transform(subject: Long) {
  private val modulus = 20201227

  private def memoize[K, V](f: K => V): K => V = {
    val cache = collection.mutable.Map.empty[K, V]
    k => cache.getOrElseUpdate(k, f(k))
  }

  private def _transform(loopSize: Int): Long =
    if (loopSize == 1)
      subject
    else {
      val lhs = loopSize / 2
      val rhs = loopSize - lhs
      (transform(lhs) * transform(rhs)) % modulus
    }

  val transform = memoize(_transform)

  def findLoopSize(target: Long, value: Long = 1, loopSize: Int = 0): Int =
    if (value == target)
      loopSize
    else
      findLoopSize(target, (value * subject) % modulus, loopSize + 1)
}

object Day25 extends App {
  def parseInput(fname: String) = {
    val lines = io.Source.fromFile(fname).getLines().toList
    (lines(0).toLong, lines(1).toLong)
  }

  val (cardPublicKey, doorPublicKey) = parseInput("inputs/input.txt")

  val doorLoopSize = Transform(7).findLoopSize(doorPublicKey)
  val answer = Transform(cardPublicKey).transform(doorLoopSize)
  println(answer)
}