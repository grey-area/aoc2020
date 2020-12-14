import io.Source

// A solution not using the Chinese remainder theorem

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

  def computeCommonTime(r1: Requirement, r2: Requirement) = {
    val newRemainder = LazyList.iterate(r1.remainder)(_ + r1.modulus).filter{
      _ % r2.modulus == r2.remainder
    }(0)

    Requirement(r1.modulus * r2.modulus, newRemainder)
  }

  val requirements = parseInput("inputs/input.txt")

  val answer = requirements.reduce(computeCommonTime).remainder
  println(answer)
}