import io.Source


// Each input differs from the one before by 1 or 3.
// Split the input wherever there's a difference of 3 into contiguous sub-lists,
// compute the answer for each sub-list, and take the product.

object Memoize {
  def apply[K, V](f: K => V): K => V = {
    val cache = collection.mutable.Map.empty[K, V]
    k => cache.getOrElseUpdate(k, f(k))
  }
}


object Day10 extends App {
  def parseInput(fname: String) = {
    val input = Source.fromFile(fname).getLines().map(_.toInt).toVector.sorted
    0 +: input :+ (3 + input.max)
  }

  def computeDifferences(vec: Vector[Int]) =
    (vec zip vec.tail) map (t => t._2 - t._1)

  def getContiguousLengths(joltages: Vector[Int]) = {
    val differences = computeDifferences(joltages)
    val gapIndices = -1 +: differences.zipWithIndex.filter(_._1 == 3).map(_._2) :+ (joltages.size - 1)
    computeDifferences(gapIndices)
  }

  def arrangementIsValid(included: Set[Int], length: Int) = {
    included.contains(1) && included.contains(length) &&
      (length == 1 || computeDifferences(included.toVector.sorted).max <= 3)
  }

  // I'm pretty sure countArrangements(n) is just the nth Tribonacci number
  def countArrangements(length: Int) = {
    BigInt((1 to length).toSet.subsets.count(arrangementIsValid(_, length)))
  }

  val joltages = parseInput("inputs/input.txt")
  val contiguousLengths = getContiguousLengths(joltages)

  val answer = contiguousLengths.map(Memoize(countArrangements)).product
  println(answer)
}