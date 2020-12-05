import scala.io.Source

object Day04 extends App {
  val input_text = Source.fromFile("inputs/input.txt").mkString

  val field_pattern = "(?:byr|iyr|eyr|hgt|hcl|ecl|pid)"
  val pattern = raw"(?:${field_pattern}:\S+\s(?:cid:\S+\s)?){7,8}".r
  val matches = pattern.findAllIn(input_text)

  println(matches.length)
}