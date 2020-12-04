import scala.io.Source

class TreeMap(input_lines: Iterator[String]) {
  def parse_line(line: String) =
    line.map(_ == '#')

  private[this] val internal_map = input_lines.toVector.map(parse_line)
  private[this] val height = internal_map.size
  private[this] val width = internal_map(0).size

  private[this] def treemap(y: Int)(x: Int) = internal_map(y)(x % width)

  def traverse_from(y: Int, x: Int, dy: Int, dx: Int, trees_hit: Int = 0): Int =
    if (y >= height) trees_hit else {
      val to_add = if (treemap(y)(x)) 1 else 0
      traverse_from(y + dy, x + dx, dy, dx, trees_hit + to_add)
    }
}

object Day03 extends App {
  def get_input_lines(filename: String) =
    Source.fromFile(filename).getLines()

  val lines = get_input_lines("inputs/input.txt")
  val treemap = new TreeMap(lines)
  val slopes = List((1, 1), (1, 3), (1, 5), (1, 7), (2, 1))

  val ans = slopes.map{
    case (dy, dx) => treemap.traverse_from(0, 0, dy, dx)
  }.foldLeft(1)(_ * _)

  println(ans)
}