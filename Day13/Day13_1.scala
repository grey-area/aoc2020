import io.Source


object Day13 extends App {
  def parseInput(fname: String) = {
    val pattern = raw"\d+".r
    val text = Source.fromFile(fname).mkString
    val values = pattern.findAllIn(text).map(_.toInt).toList
    (values.head, values.tail)
  }

  def computeWait(earliestTime: Int, busID: Int) = {
    val arriveTime = (earliestTime / busID.toFloat).ceil.toInt * busID
    arriveTime - earliestTime
  }

  val (earliestTime, busIDs) = parseInput("inputs/input.txt")

  val minPair = (busIDs.map(computeWait(earliestTime, _)) zip busIDs).min
  println(minPair._1 * minPair._2)
}