import io.Source

object Day05 extends App {
  def IDFromLine(line: String) = {
    val binaryString = line.replaceAll("B|R", "1").replaceAll("F|L", "0")
    Integer.parseInt(binaryString, 2)
  }

  val lines = Source.fromFile("input.txt").getLines()

  println(lines.map(IDFromLine).max)
}