import scala.io.Source
import scala.annotation.tailrec

final class TreeMap(input_lines: Iterator[String]) {
  private[this] def parse_line(line: String) = line.map(_ == '#')
  private[this] val internal_map = input_lines.toVector.map(parse_line)

  private[this] val height = internal_map.size
  private[this] val width = internal_map(0).size

  private[this] def treemap(y: Int)(x: Int) = internal_map(y)(x % width)

  @tailrec
  def traverse_from(y: Int, x: Int, trees_hit: Int = 0): Int =
    if (y >= height) trees_hit else {
      val to_add = if (treemap(y)(x)) 1 else 0
      traverse_from(y + 1, x + 3, trees_hit + to_add)
    }
}

object Day03 extends App {
  def get_input_lines(filename: String) =
    Source.fromFile(filename).getLines()

  val lines = get_input_lines("inputs/input.txt")
  val treemap = new TreeMap(lines)

  println(treemap.traverse_from(0, 0))
}