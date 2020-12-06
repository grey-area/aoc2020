import io.Source

object Day05 extends App {
  def IDFromLine(line: String) = {
    val binaryString = line.replaceAll("B|R", "1").replaceAll("F|L", "0")
    Integer.parseInt(binaryString, 2)
  }

  val lines = Source.fromFile("input.txt").getLines()
  val presentIDs = lines.map(IDFromLine).toSet

  val IDBeforeMine = presentIDs.find{
    x => !presentIDs.contains(x + 1) && presentIDs.contains(x + 2)
  }

  IDBeforeMine.foreach(x => println(x + 1))
}