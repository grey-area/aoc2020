import io.Source


final case class XMASProcessor(q: Vector[BigInt] = Vector[BigInt](), maxQ: Int = 25) {
  private def enqueue(e: BigInt) = {
    val newQ = q :+ e
    if (newQ.size <= maxQ)
      newQ
    else
      newQ.drop(1)
  }

  private def elementIsValid(e: BigInt) = {
    // Note: wasteful repeated computation in computing the pair sums
    val pairs = q.combinations(2)
    pairs.exists(p => p(0) + p(1) == e)
  }

  private def processElement(h: BigInt, t: List[BigInt]): Either[String, BigInt] = {
    if (q.size < maxQ || elementIsValid(h))
      XMASProcessor(enqueue(h), maxQ).processList(t)
    else
      Right(h)
  }

  def processList(l: List[BigInt]): Either[String, BigInt] =
    l match {
      case Nil => Left("No invalid entry found.")
      case h :: t => processElement(h, t)
    }
}


object Day09 extends App {
  val input = Source.fromFile("inputs/input.txt").getLines.map(BigInt(_)).toList

  val proc = XMASProcessor(maxQ = 25)
  val answer = proc.processList(input)

  answer match {
    case Left(error) => println(error)
    case Right(value) => println(f"The answer is $value.")
  }
}