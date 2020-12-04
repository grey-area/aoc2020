import scala.io.Source
import scala.util.matching.Regex


case class PasswordPolicy(character: Char, low: Int, high: Int) {
  def isValid(password: String) = {
    val characterCount = password.count(_ == character)

    (characterCount >= low) && (characterCount <= high)
  }
}


object Day02 extends App {
  def read_input(filename: String): Iterator[String] =
    Source.fromFile(filename).getLines()

  def is_line_valid(line: String): Option[Boolean] = {
    val pattern = raw"(\d*)-(\d*) ([a-z]): ([a-z]*)".r

    line match {
      case pattern(low, high, character, password) => Some(PasswordPolicy(character.charAt(0), low.toInt, high.toInt).isValid(password))
      case _ => None
    }
  }

  val lines = read_input("input.txt")

  println(lines.map(is_line_valid).flatten.count(x => x))
}