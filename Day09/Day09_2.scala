import io.Source


object Day09 extends App {
  def findConsecutiveSum(target: BigInt, remaining: Vector[BigInt], included: Vector[BigInt] = Vector[BigInt]()): Either[String, BigInt] = {
    // Note: wasteful repeated computation in computing this sum
    val sum = included.sum

    if (sum == target)
      Right(included.min + included.max)
    else if (sum < target)
      remaining match {
        case head +: tail => findConsecutiveSum(target, tail, included :+ head)
        case _ => Left("No matching consecutive sum found.")
      }
    else {
      val head +: tail = included
      findConsecutiveSum(target, remaining, tail)
    }
  }

  val input = Source.fromFile("inputs/input.txt").getLines().map(BigInt(_)).toVector
  val target = BigInt(32321523)

  val answer = findConsecutiveSum(target, input)

  answer match {
    case Left(error) => println(error)
    case Right(value) => println(f"The answer is $value.")
  }
}