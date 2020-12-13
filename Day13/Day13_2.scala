import io.Source


case class Requirement(modulus: BigInt, remainder: BigInt)


object Day13 extends App {
  def parseInput(fname: String): List[Requirement] = {
    val values = Source.fromFile(fname).mkString.split('\n')(1).split(',')
    values.zipWithIndex.filter(_._1 != "x").map{
      case (modString, rem) => {
        val mod = modString.toInt
        Requirement(mod, (-rem) % mod + mod)
      }
    }.toList
  }

  def chineseRemainderTerm(req: Requirement, N: BigInt) = {
    val Ni = N / req.modulus
    val Mi = Ni.modInverse(req.modulus)
    req.remainder * Ni * Mi
  }

  def chineseRemainder(req: List[Requirement]) = {
    val N = req.map(_.modulus).reduce(_ * _)
    req.map(chineseRemainderTerm(_, N)).sum % N
  }

  val requirements = parseInput("inputs/input.txt")
  val answer = chineseRemainder(requirements)
  println(answer)
}